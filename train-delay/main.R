source("db_connection.r")
source("upload_data.r")
source("get_statistics.r")
source("graphics.r")
source("korr_graph.r")

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

  processing_and_upload_dir("data/", con, "train_observations")

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
}

main()