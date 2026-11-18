library(dplyr)
library(ggplot2)
library(scales)

# вспомогательная функция, которая предоставляет данные
get_visualization_data <- function(
    con,
    sql_path = "sql/data_for_part_4.sql"
) {
  sql_lines <- readLines(sql_path, warn = FALSE, encoding = "UTF-8")
  sql <- paste(sql_lines, collapse = "\n")

  data <- dbGetQuery(con, sql)

  data$hour_dep <- factor(data$hour_dep)
  data$weekday_num <- factor(data$weekday_num)
  data$operator_name <- factor(data$operator_name)
  data$line_no_arr <- factor(data$line_no_arr)
  data$relation_direction <- factor(data$relation_direction)

  data
}

plot_daily_median_delay <- function(data) {
  daily_delay <- data |>
    dplyr::group_by(date_of_departure) |>
    dplyr::summarise(
      median_delay = median(delay_arr, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )

  p <- suppressWarnings(ggplot2::ggplot(
    daily_delay,
    ggplot2::aes(x = date_of_departure, y = median_delay)
  ) +
    ggplot2::geom_line(color = "#2C6FBB", linewidth = 0.8) +
    ggplot2::geom_point(color = "#2C6FBB", size = 1.6) +
    ggplot2::geom_smooth(
      method = "loess",
      se = FALSE,
      color = "#E15759",
      linewidth = 0.8,
      formula = y ~ x,
      span = 1
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) round(x / 60, 1)
    ) +
    ggplot2::labs(
      title = "Динамика медианной задержки прибытия по датам",
      subtitle = "Красная линия — сглаженный тренд",
      x = "Дата",
      y = "Медианная задержка, минуты",
      caption = "Источник: Infrabel Open Data, Raw punctuality data D-1"
    ) +
    ggplot2::theme_minimal(base_size = 11)
  )

  dir.create("output/plots", recursive = TRUE, showWarnings = FALSE)
  suppressWarnings(ggplot2::ggsave("output/plots/daily_median_delay.png", p,
                  width = 10, height = 6, dpi = 150))

  invisible(p)
}

plot_top_lines_delay_share <- function(
    data,
    top_n = 15,
    min_observations = 100
) {
  line_summary <- data |>
    dplyr::group_by(line_no_arr) |>
    dplyr::summarise(
      n = dplyr::n(),
      delay_share = mean(delay_arr > 60, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(n >= min_observations) |>
    dplyr::arrange(dplyr::desc(delay_share)) |>
    dplyr::slice_head(n = top_n)

  p <- suppressWarnings(
    ggplot2::ggplot(
      line_summary,
      ggplot2::aes(
        x = delay_share,
        y = stats::reorder(line_no_arr, delay_share)
      )
    ) +
      ggplot2::geom_col(fill = "#2C6FBB") +
      ggplot2::geom_text(
        ggplot2::aes(
          label = scales::percent(delay_share, accuracy = 0.1)
        ),
        hjust = -0.1,
        size = 3.2
      ) +
      ggplot2::scale_x_continuous(
        labels = scales::percent,
        limits = c(0, 1)
      ) +
      ggplot2::labs(
        title = "Топ-15 линий по доле задержек прибытия более 60 секунд",
        subtitle = sprintf(
          "Учтены линии с числом наблюдений не менее %d",
          min_observations
        ),
        x = "Доля остановок с задержкой более 60 секунд",
        y = "Линия прибытия",
        caption = "Источник: Infrabel Open Data, Raw punctuality data D-1"
      ) +
      ggplot2::theme_minimal(base_size = 11)
  )

  dir.create("output/plots", recursive = TRUE, showWarnings = FALSE)
  suppressWarnings(
    ggplot2::ggsave(
      "output/plots/top_lines_delay_share.png",
      p,
      width = 10,
      height = 6,
      dpi = 150
    )
  )

  invisible(p)
}