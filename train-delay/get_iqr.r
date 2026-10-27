source("db_connection.r")
library(knitr)

con <- connect_db()
on.exit(dbDisconnect(con), add = TRUE)

get_iqr_bounds <- function(con,
                           sql_path = "sql/iqr_bounds.sql") {
  sql <- paste(
    readLines(sql_path, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  dbGetQuery(con, sql)
}

iqr <- get_iqr_bounds(con)
print(knitr::kable(iqr, format = "simple"))