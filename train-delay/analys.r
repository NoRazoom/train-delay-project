library(broom)
library(parsnip)


test_delay_by_hour <- function(con,
                              sql_path = "sql/params_to_delay/hour_to_delay.sql") {
  sql <- paste(
    readLines(sql_path, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  data <- dbGetQuery(con, sql)

  data <- data[complete.cases(data[, c("delay_arr", "hour_dep")]), ]
  data$hour_dep <- factor(data$hour_dep)

  # ранговый непараметрический тест Краскела-Уоллиса
  kt <- kruskal.test(delay_arr ~ hour_dep, data = data)

  n_total <- nrow(data)
  k_groups <- nlevels(data$hour_dep)

  epsilon_squared <- (kt$statistic - k_groups + 1) /
    (n_total - k_groups)

  medians <- data |>
    group_by(hour_dep) |>
    summarise(
      n      = n(),
      median = median(delay_arr),
      .groups = "drop"
    ) |>
    arrange(desc(median))

  top <- medians[1, ]

  list(
    test       = tidy(kt),
    medians    = medians,
    top_hour   = as.character(top$hour_dep),
    top_median = top$median,
    epsilon_squared = unname(epsilon_squared)
  )
}


test_delay_by_weekday <- function(con,
                                  sql_path = "sql/params_to_delay/weekday_to_delay.sql") {
  sql <- paste(
    readLines(sql_path, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  data <- dbGetQuery(con, sql)

  data <- data[complete.cases(data[, c("delay_arr", "weekday_num")]), ]
  data$weekday_num <- factor(data$weekday_num)

  # ранговый непараметрический тест Краскела-Уоллиса
  kt <- kruskal.test(delay_arr ~ weekday_num, data = data)

  n_total <- nrow(data)
  k_groups <- nlevels(data$weekday_num)

  epsilon_squared <- (kt$statistic - k_groups + 1) /
    (n_total - k_groups)

  medians <- data |>
    group_by(weekday_num) |>
    summarise(
      n      = n(),
      median = median(delay_arr),
      .groups = "drop"
    ) |>
    arrange(desc(median))

  top <- medians[1, ]

  list(
    test        = tidy(kt),
    medians     = medians,
    top_weekday = as.character(top$weekday_num),
    top_median  = top$median,
    epsilon_squared = unname(epsilon_squared)
  )
}


test_delay_by_operator <- function(con,
                                   sql_path = "sql/params_to_delay/operator_to_delay.sql") {
  sql <- paste(
    readLines(sql_path, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  data <- dbGetQuery(con, sql)

  data <- data[complete.cases(data[, c("delay_arr", "operator_name")]), ]
  data$operator_name <- factor(data$operator_name)

  # ранговый непараметрический тест Краскела-Уоллиса
  kt <- kruskal.test(delay_arr ~ operator_name, data = data)

  n_total <- nrow(data)
  k_groups <- nlevels(data$operator_name)

  epsilon_squared <- (kt$statistic - k_groups + 1) /
    (n_total - k_groups)

  medians <- data |>
    group_by(operator_name) |>
    summarise(
      n      = n(),
      median = median(delay_arr),
      .groups = "drop"
    ) |>
    arrange(desc(median))

  top <- medians[1, ]

  list(
    test         = tidy(kt),
    medians      = medians,
    top_operator = as.character(top$operator_name),
    top_median   = top$median,
    epsilon_squared = unname(epsilon_squared)
  )
}

test_delay_by_line <- function(con,
                               sql_path = "sql/params_to_delay/line_to_delay.sql") {
  sql <- paste(
    readLines(sql_path, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  data <- dbGetQuery(con, sql)

  data <- data[complete.cases(data[, c("delay_arr", "line_no_arr")]), ]
  data$line_no_arr <- factor(data$line_no_arr)

  # ранговый непараметрический тест Краскела-Уоллиса
  kt <- kruskal.test(delay_arr ~ line_no_arr, data = data)

  n_total <- nrow(data)
  k_groups <- nlevels(data$line_no_arr)

  epsilon_squared <- (kt$statistic - k_groups + 1) /
    (n_total - k_groups)

  medians <- data |>
    group_by(line_no_arr) |>
    summarise(
      n      = n(),
      median = median(delay_arr),
      .groups = "drop"
    ) |>
    arrange(desc(median))

  top <- medians[1, ]

  list(
    test       = tidy(kt),
    medians    = medians,
    top_line   = as.character(top$line_no_arr),
    top_median = top$median,
    epsilon_squared = unname(epsilon_squared)
  )
}

build_delay_prediction_model <- function(con,
                                  sql_path = "sql/params_to_delay/delay_model_data.sql") {
  sql <- paste(
    readLines(sql_path, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  data <- dbGetQuery(con, sql)

  # удаляем NA
  data <- data[complete.cases(data), ]

  data$hour_dep <- factor(data$hour_dep)
  data$weekday_num <- factor(data$weekday_num)
  data$operator_name <- factor(data$operator_name)
  data$line_no_arr <- factor(data$line_no_arr)
  data$relation_direction <- factor(data$relation_direction)

  # сортировка 
  data <- data |>
    arrange(date_of_departure)

  # 80-й процентиль даты
  split_date <- quantile(
    data$date_of_departure,
    probs = 0.8,
    type = 1
  )

  # распределение данных по процентилю на тестовую и тренировочную выборки
  train_data <- data |>
    filter(date_of_departure <= split_date)

  test_data <- data |>
    filter(date_of_departure > split_date)

  # азовое предсказание
  baseline_prediction <- median(train_data$delay_arr)

  # средняя абсолютная ошибка
  baseline_mae <- mean(
    abs(test_data$delay_arr - baseline_prediction)
  )

  #  ещё не обученная модель (конструктор спецификации)
  rf_model <- rand_forest(
    trees = 100,
    min_n = 10,
    mtry = 2
  ) |>
    set_engine("ranger") |>
    set_mode("regression")
 
  # обучение модели
  rf_fit <- rf_model |>
    fit(
      delay_arr ~ hour_dep +
        weekday_num +
        operator_name +
        line_no_arr +
        relation_direction,
      data = train_data
    )

  # Предсказание на тесте
  predictions <- predict(rf_fit, test_data) |>
    pull(.pred)

  model_mae <- mean(
    abs(test_data$delay_arr - predictions)
  )

  # Улучшение
  improvement <- baseline_mae - model_mae

  list(
    baseline_mae = baseline_mae,
    model_mae = model_mae,
    improvement = improvement,
    improvement_pct = 100 * improvement / baseline_mae,
    n_train = nrow(train_data),
    n_test = nrow(test_data),
    split_date = split_date
  )
}