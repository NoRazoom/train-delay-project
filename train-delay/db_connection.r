library(DBI)
library(RPostgres)
library(dotenv)

load_dot_env()

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("DB_NAME"),
  host = Sys.getenv("DB_HOST"),
  # as - семейство функций преобразования типов
  port = as.integer(Sys.getenv("DB_PORT")),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

# Проверка подключения
dbListTables(con)
print(dbGetQuery(con, "SELECT version()"))
# Закрытие соединения
dbDisconnect(con)