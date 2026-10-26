suppressPackageStartupMessages(library(httr2))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(DBI))

export_url <- paste0(
  "https://opendata.infrabel.be/api/explore/v2.1/catalog/datasets/",
  "ruwe-gegevens-van-stiptheid-d-1/exports/csv"
)


# """Функция скачивания и сохранения CSV"""
download_and_save_csv <- function(output_dir = "data/", file_name = NULL) {
  
  export_url <- paste0(
    "https://opendata.infrabel.be/api/explore/v2.1/catalog/datasets/",
    "ruwe-gegevens-van-stiptheid-d-1/exports/csv"
  )
  
  # создание папки
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  if (is.null(file_name)) {
    file_name <- paste0("infrabel_", Sys.Date(), ".csv")
  }
  file_path <- file.path(output_dir, file_name)
  
  message("Скачивание данных с Infrabel Open Data...")
  
  response <- request(export_url) |>
    req_error(is_error = function(resp) resp_status(resp) >= 400) |>
    req_perform()
  
  # запись всех записей в файл
  write_lines(
    resp_body_string(response),
    file_path
  )
  
  message("Данные сохранены в: ", file_path, "\n")
}


# """Функция подготовки данных под схему БД"""
prepare_data_for_db <- function(raw_data) {
  
  message("Подготовка данных под схему БД...")
  
  # переименование полей под схему БД
  data <- raw_data %>%
    rename(
      date_of_departure = datdep,
      train_num = train_no,
      relation = relation,
      railway_operator = train_serv,
      departure_line = line_no_dep,
      arr_line = line_no_arr,
      real_arrival_time = real_time_arr,
      real_departure_time = real_time_dep,
      planned_arrival_time = planned_time_arr,
      planned_departure_time = planned_time_dep,
      delay_arr = delay_arr,
      delay_dep = delay_dep,
      relation_direction = relation_direction,
      stopping_place = ptcar_lg_nm_nl
    )
  
  message(" 1.Поля переименованы")
  
  data <- data %>%
    mutate(
      # даты
      date_of_departure = as.Date(date_of_departure),
      
      # числа
      train_num = as.numeric(train_num),
      departure_line = as.character(departure_line),
      arr_line = as.character(arr_line),
      delay_arr = as.integer(delay_arr),
      delay_dep = as.integer(delay_dep),
      
      # текст
      relation = as.character(relation),
      railway_operator = as.character(railway_operator),
      relation_direction = as.character(relation_direction),
      stopping_place = as.character(stopping_place),
      
      # время: из "HH:MM" в TIMESTAMP (сегодняшняя дата + время)
      planned_departure_time = as.POSIXct(
        paste(as.Date(Sys.Date()), planned_departure_time),
        format = "%Y-%m-%d %H:%M"
      ),
      planned_arrival_time = as.POSIXct(
        paste(as.Date(Sys.Date()), planned_arrival_time),
        format = "%Y-%m-%d %H:%M"
      ),
      real_departure_time = as.POSIXct(
        paste(as.Date(Sys.Date()), real_departure_time),
        format = "%Y-%m-%d %H:%M"
      ),
      real_arrival_time = as.POSIXct(
        paste(as.Date(Sys.Date()), real_arrival_time),
        format = "%Y-%m-%d %H:%M"
      )
    )
  
  message(" 2.Типы данных настроены")

  n_before_delay <- nrow(data)
  data <- data %>%
    filter(!is.na(delay_arr), !is.na(delay_dep))
  n_after_delay <- nrow(data)
  
  if (n_before_delay != n_after_delay) {
    message(" 2.1.Удалено строк с NA в delay_arr/delay_dep: ",
            n_before_delay - n_after_delay)
  } else {
    message(" 2.1.Пропусков в delay_arr/delay_dep не обнаружено")
  }
  
  data <- data %>%
    mutate(
      planned_date_dep = as.Date(planned_departure_time),
      planned_date_arr = as.Date(planned_arrival_time),
      real_date_dep = as.Date(real_departure_time),
      real_date_arr = as.Date(real_arrival_time)
    )
  
  message(" 3.Извлечены даты из временных полей")
  
  n_before <- nrow(data)
  data <- data %>%
    distinct(date_of_departure, train_num, stopping_place,
             planned_arrival_time, planned_departure_time, .keep_all = TRUE)
  n_after <- nrow(data)
  
  if (n_before != n_after) {
    message(" 4.Удалено дубликатов: ", n_before - n_after)
  }
  
  na_summary <- colSums(is.na(data))
  if (any(na_summary > 0)) {
    message("\nПропуски в столбцах:\n")
    print(na_summary[na_summary > 0])
  } else {
    message("\nПропусков не осталось")
  }
  
  
  message("\nДанные готовы к загрузке в БД")
  
  data
}


# """Функция загрузки данных в БД"""
upload_to_db <- function(data, con, table_name = "train_observations") {
  
  message("\nЗагрузка в таблицу: ", table_name)
  
  # Загрузка (overwrite = TRUE заменит таблицу)
  # dbWriteTable(con, table_name, data, overwrite = FALSE, append = TRUE, row.names = FALSE)
  dbWriteTable(con, table_name, data, overwrite = TRUE, row.names = FALSE)
  
  # Проверка
  n_rows <- dbGetQuery(con, paste0("SELECT COUNT(*) as n FROM ", table_name))$n
  message("Загружено строк: ", n_rows)
}

# """Функция чтения всех данных"""
read_all_csv <- function(input_dir = "data/", pattern = "\\.csv$") {
  files <- list.files(input_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    stop("В папке ", input_dir, " не найдено CSV-файлов")
  }
  message("Найдено файлов: ", length(files))

    read_one <- function(path) {
      message("  читаю: ", basename(path))
      suppressWarnings(suppressMessages(
        read_csv2(
          path,
          col_types = cols(.default = col_character()),
          show_col_types = FALSE
        )
      ))
    }

  bind_rows(lapply(files, read_one))
}

# """Функция объединябщая чтение, обработку и загрузку данных"""
processing_and_upload_dir <- function(input_dir, con,
                                      table_name = "train_observations") {
  raw_data <- read_all_csv(input_dir)

  prepared_data <- prepare_data_for_db(raw_data)

  upload_to_db(prepared_data, con, table_name)

  invisible(prepared_data)
}