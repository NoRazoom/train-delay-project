source("db_connection.r")

main <- function() {
  con <- connect_db()
  on.exit(dbDisconnect(con), add = TRUE)

  apply_schema(con)
}

main()