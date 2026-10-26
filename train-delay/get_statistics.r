library(DBI)
library(knitr)

get_delay_statistics <- function(con) {
  sql <- paste(
    readLines("sql/delay_statistics.sql", warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  res <- dbGetQuery(con, sql)
}