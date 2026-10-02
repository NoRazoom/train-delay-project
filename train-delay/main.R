source("db_connection.r")
source("upload_data.r")

main <- function() {
  con <- connect_db()
  on.exit(dbDisconnect(con), add = TRUE)

  apply_schema(con)
  file_path <- download_and_save_csv()
  processing_and_upload_data(file_path, con)
}

main()