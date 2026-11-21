library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(readr)

model_data <- read_csv(
  "fulldata.csv",
  show_col_types = FALSE
)

model_data$date_of_departure <- as.Date(
  model_data$date_of_departure
)

model_data$hour_dep <- factor(model_data$hour_dep)
model_data$operator_name <- factor(model_data$operator_name)
model_data$line_no_arr <- factor(model_data$line_no_arr)
model_data$relation_direction <- factor(model_data$relation_direction)

ui <- fluidPage(
  titlePanel("Анализ задержек прибытия пассажирских поездов"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        "operator",
        "Оператор:",
        choices = c(
          "Все операторы",
          sort(unique(model_data$operator_name))
        )
      ),

      selectInput(
        "line",
        "Линия прибытия:",
        choices = c(
          "Все линии",
          sort(unique(model_data$line_no_arr))
        )
      ),

      selectInput(
        "direction",
        "Направление движения:",
        choices = c(
          "Все направления",
          sort(unique(model_data$relation_direction))
        )
      ),

      selectInput(
        "hour",
        "Час отправления:",
        choices = c(
          "Все часы",
          sort(unique(model_data$hour_dep))
        )
      ),

      selectInput(
        "metric",
        "Показатель для графика по часам:",
        choices = c(
          "Медианная задержка" = "median",
          "Средняя задержка" = "mean",
          "Доля задержек более 60 с" = "share"
        )
      )
    ),

    mainPanel(
      verbatimTextOutput("summary"),

      plotOutput("hour_plot", height = "360px"),

      plotOutput("daily_plot", height = "360px")
    )
  )
)

server <- function(input, output, session) {

  filtered_data <- reactive({
    data <- model_data

    if (input$operator != "Все операторы") {
      data <- filter(data, operator_name == input$operator)
    }

    if (input$line != "Все линии") {
      data <- filter(data, line_no_arr == input$line)
    }

    if (input$direction != "Все направления") {
      data <- filter(
        data,
        relation_direction == input$direction
      )
    }

    if (input$hour != "Все часы") {
      data <- filter(data, hour_dep == input$hour)
    }

    data
  })

  output$summary <- renderText({
    data <- filtered_data()

    if (nrow(data) == 0) {
      return("Для выбранных параметров данные отсутствуют.")
    }

    median_delay <- median(data$delay_arr, na.rm = TRUE)
    mean_delay <- mean(data$delay_arr, na.rm = TRUE)
    share_over_60 <- 100 * mean(
      data$delay_arr > 60,
      na.rm = TRUE
    )

    sprintf(
      "Наблюдений: %d\nМедианная задержка: %.2f секунд\nСредняя задержка: %.2f секунд\nДоля задержек более 60 с: %.1f%%",
      nrow(data),
      median_delay,
      mean_delay,
      share_over_60
    )
  })

  output$hour_plot <- renderPlot({
    data <- filtered_data()

    validate(
      need(
        nrow(data) > 0,
        "Для выбранных параметров данные отсутствуют."
      )
    )

    if (input$metric == "share") {
      plot_data <- data |>
        group_by(hour_dep) |>
        summarise(
          value = mean(delay_arr > 60, na.rm = TRUE),
          .groups = "drop"
        )

      ggplot(plot_data, aes(x = factor(hour_dep), y = value)) +
        geom_col(fill = "#2C6FBB") +
        scale_y_continuous(
          labels = function(x) paste0(round(x / 60, 1), " мин")
        ) +
        labs(
          title = "Доля задержек более 60 секунд по часам отправления",
          x = "Час отправления",
          y = "Доля наблюдений"
        ) +
        theme_minimal(base_size = 11)
    } else {
      plot_data <- data |>
        group_by(hour_dep) |>
        summarise(
          value = if (input$metric == "median") {
            median(delay_arr, na.rm = TRUE)
          } else {
            mean(delay_arr, na.rm = TRUE)
          },
          .groups = "drop"
        )

      ggplot(plot_data, aes(x = factor(hour_dep), y = value)) +
        geom_col(fill = "#2C6FBB") +
        scale_y_continuous(
          labels = function(x) paste0(round(x / 60, 1), " мин")
        ) +
        labs(
          title = "Задержка прибытия по часам отправления",
          subtitle = "Значения приведены в минутах",
          x = "Час отправления",
          y = "Задержка, минуты"
        ) +
        theme_minimal(base_size = 11)
    }
  })

  output$daily_plot <- renderPlot({
    data <- filtered_data()

    validate(
      need(
        nrow(data) > 0,
        "Для выбранных параметров данные отсутствуют."
      )
    )

    daily_data <- data |>
      group_by(date_of_departure) |>
      summarise(
        median_delay = median(delay_arr, na.rm = TRUE),
        .groups = "drop"
      )

    ggplot(
      daily_data,
      aes(x = date_of_departure, y = median_delay)
    ) +
      geom_line(color = "#2C6FBB", linewidth = 0.8) +
      geom_point(color = "#2C6FBB", size = 1.6) +
      scale_y_continuous(
        labels = function(x) paste0(round(x / 60, 1), " мин")
      ) +
      labs(
        title = "Медианная задержка прибытия по датам",
        subtitle = "Для выбранных параметров фильтрации",
        x = "Дата",
        y = "Медианная задержка, минуты"
      ) +
      theme_minimal(base_size = 11)
  })
}

shinyApp(ui = ui, server = server)