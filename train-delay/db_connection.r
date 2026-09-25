library(DBI)
library(RPostgres)
library(dotenv)

load_dot_env()

connect_db <- function() {
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname   = Sys.getenv("DB_NAME"),
    host     = Sys.getenv("DB_HOST"),
    # as - семейство функций преобразования типов
    port     = as.integer(Sys.getenv("DB_PORT")),
    user     = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
  message("Подключение к бд установлено.")
  con
}

apply_schema <- function(con, sql_file = "sql/create_schema.sql") {
  if (!file.exists(sql_file)) {
    stop("Не найден файл схемы: ", sql_file)
  }
  sql <- paste(readLines(sql_file, encoding = "UTF-8"), collapse = "\n")
  # обрезаем пробелы и сплитуем
  statements <- trimws(strsplit(sql, ";")[[1]])
  # только непустые запросы
  statements <- statements[nzchar(statements)]

  for (stmt in statements) {
    dbExecute(con, stmt)
  }
  message("Успешное подключение к таблице.")
}
