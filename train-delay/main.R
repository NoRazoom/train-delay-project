source("db_connection.r")
source("upload_data.r")
source("get_statistics.r")
source("graphics.r")
source("korr_graph.r")
source("analys.r")
source("visual_for_part_4.r")

# ожидание ввода в неитерактивных терминалах
user_input <- function(prompt) {
  if (interactive()) {
    return(readline(prompt))
  } else {
    cat(prompt)
    return(readLines("stdin", n = 1))
  }
}

main <- function() {

  con <- connect_db()
  on.exit(dbDisconnect(con), add = TRUE)

  apply_schema(con)

  is_need_upload_data <- user_input("Загрузить новые данные? да/нет (Только при подключении к интернету)")
  if (tolower(trimws(is_need_upload_data)) == "да") {
    download_and_save_csv()
  }

  is_need_upload_data_to_db <- user_input("Загрузить данные в бд? да/нет")
  if (tolower(trimws(is_need_upload_data)) == "да") {
    processing_and_upload_dir("data/", con, "train_observations")
  }
  is_need_stat <- user_input("Вывести статистику? да/нет: ")
  if (tolower(trimws(is_need_stat)) == "да") {
    stats <- get_delay_statistics(con)
    # красивый вывод - библиотека knitr
    print(knitr::kable(stats, format = "simple"))
  }

  action <- user_input("Выгрузить графики? да/нет: ")
  if (tolower(trimws(action)) == "да") {
    get_delay_gist(con)
    plot_delay_boxplot(con)
    plot_weekday_delay(con)
    message("\nВсе графики выгружены в /output/plots\n")
  }

  is_need_korr <- user_input("Произвести рассчет корреляции? да/нет: ")
  if (tolower(trimws(is_need_korr)) == "да") {
    get_delay_correlation(con)
    message("\n")
  }

  is_need_analys <- user_input("Вывести результаты анализа зависимостей опозданий от параметров? да/нет: ")
  if (tolower(trimws(is_need_korr)) == "да") {
    result <- test_delay_by_hour(con)
    print(result$test)
    print(result$top_hour)
    print(result$top_median)
    print(result$epsilon_squared)

    weekday_result <- test_delay_by_weekday(con)
    print(weekday_result$test)
    print(weekday_result$top_weekday)
    print(weekday_result$top_median)
    print(weekday_result$epsilon_squared)


    operator_result <- test_delay_by_operator(con)
    print(operator_result$test)
    print(operator_result$top_operator)
    print(operator_result$top_median)
    print(operator_result$epsilon_squared)
    

    line_result <- test_delay_by_line(con)
    print(line_result$test)
    print(line_result$top_line)
    print(line_result$top_median)
    print(line_result$epsilon_squared)
    message("\n")
  }

  is_need_model <- user_input("Построить модель предсказаний опозданий? +/-:")
  if (tolower(trimws(is_need_model)) == "+") {
    model_result <- build_delay_prediction_model(con)

    cat(sprintf(
      "Дата разделения выборки: %s\n",
      as.character(model_result$split_date)
    ))

    cat(sprintf(
      "Наблюдений для обучения: %s\n",
      format(model_result$n_train, big.mark = " ", scientific = FALSE)
    ))

    cat(sprintf(
      "Наблюдений для тестирования: %s\n\n",
      format(model_result$n_test, big.mark = " ", scientific = FALSE)
    ))

    cat(sprintf(
      "MAE базового прогноза (медиана): %.2f сек.\n",
      model_result$baseline_mae
    ))

    cat(sprintf(
      "MAE случайного леса:            %.2f сек.\n",
      model_result$model_mae
    ))

    cat(sprintf(
      "Уменьшение ошибки:              %.2f сек.\n",
      model_result$improvement
    ))

    cat(sprintf(
      "Относительное улучшение:        %.2f %%\n",
      model_result$improvement_pct
    ))
  }

  visualization_data <- get_visualization_data(con)

  plot_path <- plot_daily_median_delay(visualization_data)
  plot_path <- plot_top_lines_delay_share(visualization_data)
}

main()