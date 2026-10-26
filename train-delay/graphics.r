library(dplyr)
library(ggplot2)
library(lubridate)
suppressPackageStartupMessages(library(scales))
library(ragg)
library(tidyr)

# """Функция построения графика гистограммы отклонений прибытий"""
get_delay_gist <- function(con) {
    sql <- paste(
        readLines("sql/raw_data.sql", warn = FALSE, encoding = "UTF-8"),
        collapse = "\n"
    )
    delays <- dbGetQuery(con, sql)

    delays <- delays %>%
        mutate(
            PLANNED_TIME_DEP = as.POSIXct(planned_date_dep, tz = "Europe/Brussels"),
            DATE = as.Date(date_of_departure),
            delay_arr_min = delay_arr / 60,
            delay_dep_min = delay_dep / 60,
            hour = hour(planned_departure_time),
            weekday = wday(date_of_departure, label = TRUE, week_start = 1)
        )

    p <- ggplot(delays, aes(x = delay_arr_min)) +
        geom_histogram(bins = 60, fill = "#4C72B0", color = "white") +
        geom_vline(
            aes(xintercept = median(delay_arr_min, na.rm = TRUE)),
            linetype = "dashed", color = "red"
        ) +
        coord_cartesian(xlim = c(-20, 45)) +
        labs(
            title = "Распределение задержек прибытия",
            x = "Задержка прибытия, минуты",
            y = "Количество наблюдений"
        ) +
        theme_minimal(base_family = "Arial")
    
    dir.create("output/plots", recursive = TRUE, showWarnings = FALSE)

    suppressWarnings(
        suppressMessages(
            ggsave(
                "output/plots/delay_arr_histogram.png",
                plot = p,
                device = ragg::agg_png,
                width = 10,
                height = 6,
                dpi = 300
            )
        )
    )

    message("Гистаграма распределения отклонений прибытий сохранена.")

    invisible(p)
}

# """Функция построения графика коробки с усами отклонений прибытий и отправлений"""
plot_delay_boxplot <- function(con,
                               limit_min = 80) {

  sql <- paste(
    readLines("sql/only_arr_dep.sql", warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  delays <- dbGetQuery(con, sql)

  # приведение к длинному виду
  long <- delays %>%
    pivot_longer(cols = everything(),
                 names_to = "type",
                 values_to = "delay_min") %>%
    mutate(type = recode(type,
                         delay_arr_min = "Прибытие",
                         delay_dep_min = "Отправление"))

  # фильтровка выбросов
  long_plot <- long %>%
    filter(delay_min >= -limit_min, delay_min <= limit_min)


  p <- ggplot(long_plot, aes(x = type, y = delay_min, fill = type)) +
    geom_boxplot(
      width = 0.5,
      outlier.shape = 16,
      outlier.size = 1.5,
      outlier.alpha = 0.5,
      outlier.colour = "red"
    ) +
    labs(
      title = "Распределение задержек прибытия и отправления",
      subtitle = paste0("Показан диапазон ±", limit_min, " минут"),
      x = NULL,
      y = "Задержка, минуты"
    ) +
    theme_minimal(base_family = "Arial") +
    theme(legend.position = "none")

  dir.create(dirname("output/plots"), recursive = TRUE, showWarnings = FALSE)
  suppressWarnings(
    ggsave("output/plots/delay_arr_dep_boxplot.png", plot = p, device = ragg::agg_png,
           width = 10, height = 6, dpi = 300)
  )
  message("Боксплот задержек отправлений и прибытий сохранён.")

  invisible(p)
}

# """Функция построения графика столбцатой диаграммы задержек прибытий по дням недели"""
plot_weekday_delay <- function(con,
                               sql_path = "sql/weekday_delay.sql",
                               out_path = "output/plots/weekday_delay.png") {
  sql <- paste(
    readLines(sql_path, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  stats <- dbGetQuery(con, sql)

  stats$weekday <- factor(
    stats$weekday,
    levels = c("Понедельник", "Вторник", "Среда", "Четверг",
               "Пятница", "Суббота", "Воскресенье")
  )

  p <- ggplot(stats, aes(x = weekday, y = mean_delay_arr_min)) +
    geom_col(fill = "#4C72B0", color = "white", width = 0.7) +
    geom_text(
      aes(label = sprintf("%.1f", mean_delay_arr_min)),
      vjust = -0.5, size = 3.5
    ) +
    labs(
      title = "Средняя задержка прибытия по дням недели",
      x = NULL,
      y = "Средняя задержка, минуты"
    ) +
    theme_minimal(base_family = "Arial")

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  suppressWarnings(
    ggsave(out_path, plot = p, device = ragg::agg_png,
           width = 10, height = 6, dpi = 300)
  )
  message("Столбчатая диаграмма по дням недели сохранена.")

  invisible(p)
}
