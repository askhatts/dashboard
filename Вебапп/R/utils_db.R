# ============================================================
# УТИЛИТЫ ДЛЯ РАБОТЫ С ДАННЫМИ (SQLite)
# ============================================================
# Все данные хранятся в SQLite базе data/abai_region.sqlite.
# Скрининг и эпидемиология — в длинном формате (indicator + value),
# что позволяет работать с произвольными столбцами из Excel.
# ============================================================

DB_PATH <- file.path("data", "abai_region.sqlite")

# === ПОДКЛЮЧЕНИЕ К БД ===
get_db_connection <- function(db_path = DB_PATH) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(conn, "PRAGMA foreign_keys = ON")
  DBI::dbExecute(conn, "PRAGMA journal_mode = WAL")
  ensure_tables(conn)
  conn
}

# === СОЗДАНИЕ GENERIC-ТАБЛИЦ ===
ensure_tables <- function(conn) {
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS screening_data (
      scr_id INTEGER PRIMARY KEY AUTOINCREMENT,
      mo_id INTEGER NOT NULL,
      data_year INTEGER NOT NULL,
      data_month INTEGER,
      screening_type TEXT NOT NULL DEFAULT '',
      indicator TEXT NOT NULL,
      value REAL,
      import_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      import_batch_id TEXT
    )
  ")
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS epidemiology_data (
      epi_id INTEGER PRIMARY KEY AUTOINCREMENT,
      district_id INTEGER NOT NULL,
      data_year INTEGER NOT NULL,
      data_month INTEGER,
      indicator TEXT NOT NULL,
      value REAL,
      import_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      import_batch_id TEXT
    )
  ")
}

# === ЗАГРУЗКА РАЙОНОВ ===
# Возвращает sf-объект с полигонами районов.
load_districts <- function(conn) {
  tryCatch({
    df <- DBI::dbGetQuery(conn, "
      SELECT district_id, district_name_en, district_name_ru, district_pcode,
             district_type, area_km2, population, geometry_wkt,
             centroid_lat, centroid_lon
      FROM districts ORDER BY district_id
    ")
    if (nrow(df) == 0) return(NULL)

    # Конвертация WKT → sf
    valid <- !is.na(df$geometry_wkt) & nchar(df$geometry_wkt) > 0
    if (!any(valid)) return(NULL)

    geom <- sf::st_as_sfc(df$geometry_wkt[valid], crs = 4326)
    df_data <- df[valid, setdiff(names(df), "geometry_wkt"), drop = FALSE]
    sf_obj <- sf::st_sf(df_data, geometry = geom, crs = 4326)
    sf_obj
  }, error = function(e) {
    warning("Ошибка загрузки районов из SQLite: ", e$message)
    NULL
  })
}

# === ЗАГРУЗКА МЕДОРГАНИЗАЦИЙ ===
# Возвращает data.frame с координатами и атрибутами МО (денормализованный).
load_mo <- function(conn) {
  tryCatch({
    df <- DBI::dbGetQuery(conn, "
      SELECT m.mo_id, m.mo_name, m.mo_short_name, m.mo_name_normalized,
             m.mo_type, m.ownership, m.latitude, m.longitude,
             d.district_id, d.district_name_ru
      FROM medical_organizations m
      LEFT JOIN mo_district_link mdl ON m.mo_id = mdl.mo_id AND mdl.is_primary = 1
      LEFT JOIN districts d ON mdl.district_id = d.district_id
      WHERE m.active = 1
      ORDER BY m.mo_id
    ")
    if (nrow(df) == 0) return(create_empty_mo())
    df
  }, error = function(e) {
    warning("Ошибка загрузки МО из SQLite: ", e$message)
    create_empty_mo()
  })
}

# === ЗАГРУЗКА ЭПИДЕМИОЛОГИИ ===
# Возвращает data.frame в длинном формате (indicator + value).
load_epidemiology <- function(conn) {
  tryCatch({
    df <- DBI::dbGetQuery(conn, "
      SELECT epi_id, district_id, data_year AS year, data_month AS month,
             indicator, value, import_date
      FROM epidemiology_data
      ORDER BY data_year, data_month
    ")
    if (nrow(df) == 0) return(create_empty_epidemiology())
    df
  }, error = function(e) {
    warning("Ошибка загрузки эпидемиологии из SQLite: ", e$message)
    create_empty_epidemiology()
  })
}

# === ЗАГРУЗКА СКРИНИНГА ===
# Возвращает data.frame в длинном формате (indicator + value).
# Если screening_type непустой, добавляет префикс к indicator.
load_screening <- function(conn) {
  tryCatch({
    df <- DBI::dbGetQuery(conn, "
      SELECT scr_id, mo_id, data_year AS year, data_month AS month,
             CASE WHEN screening_type != '' THEN screening_type || ' - ' || indicator
                  ELSE indicator END AS indicator,
             value, import_date
      FROM screening_data
      ORDER BY data_year, data_month
    ")
    if (nrow(df) == 0) return(create_empty_screening())
    df
  }, error = function(e) {
    warning("Ошибка загрузки скрининга из SQLite: ", e$message)
    create_empty_screening()
  })
}

# === CRUD: ЭПИДЕМИОЛОГИЯ ===

save_epi_row <- function(conn, district_id, year, month, indicator, value) {
  DBI::dbExecute(conn, "
    INSERT INTO epidemiology_data (district_id, data_year, data_month, indicator, value)
    VALUES (?, ?, ?, ?, ?)
  ", params = list(district_id, year, month, indicator, value))
}

update_epi_cell <- function(conn, epi_id, column, value) {
  allowed <- c("district_id", "data_year", "data_month", "indicator", "value")
  if (!column %in% allowed) return(FALSE)
  sql <- paste0("UPDATE epidemiology_data SET ", column, " = ? WHERE epi_id = ?")
  DBI::dbExecute(conn, sql, params = list(value, epi_id))
  TRUE
}

delete_epi_rows <- function(conn, epi_ids) {
  if (length(epi_ids) == 0) return(0)
  placeholders <- paste(rep("?", length(epi_ids)), collapse = ", ")
  sql <- paste0("DELETE FROM epidemiology_data WHERE epi_id IN (", placeholders, ")")
  DBI::dbExecute(conn, sql, params = as.list(epi_ids))
}

insert_epi_from_import <- function(conn, long_data) {
  if (is.null(long_data) || nrow(long_data) == 0) return(0)
  batch_id <- paste0("epi_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  n <- nrow(long_data)
  insert_df <- data.frame(
    district_id     = as.integer(long_data$entity_id),
    data_year       = as.integer(long_data$year),
    data_month      = as.integer(long_data$month),
    indicator       = as.character(long_data$indicator),
    value           = as.numeric(long_data$value),
    import_date     = rep(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), n),
    import_batch_id = rep(batch_id, n),
    stringsAsFactors = FALSE
  )
  insert_df <- insert_df[!is.na(insert_df$district_id), , drop = FALSE]
  if (nrow(insert_df) == 0) return(0)

  DBI::dbBegin(conn)
  tryCatch({
    DBI::dbAppendTable(conn, "epidemiology_data", insert_df)
    DBI::dbCommit(conn)
    nrow(insert_df)
  }, error = function(e) {
    DBI::dbRollback(conn)
    stop(e)
  })
}

# === CRUD: СКРИНИНГ ===

save_scr_row <- function(conn, mo_id, year, month, screening_type, indicator, value) {
  DBI::dbExecute(conn, "
    INSERT INTO screening_data (mo_id, data_year, data_month, screening_type, indicator, value)
    VALUES (?, ?, ?, ?, ?, ?)
  ", params = list(mo_id, year, month, screening_type, indicator, value))
}

update_scr_cell <- function(conn, scr_id, column, value) {
  allowed <- c("mo_id", "data_year", "data_month", "screening_type", "indicator", "value")
  if (!column %in% allowed) return(FALSE)
  sql <- paste0("UPDATE screening_data SET ", column, " = ? WHERE scr_id = ?")
  DBI::dbExecute(conn, sql, params = list(value, scr_id))
  TRUE
}

delete_scr_rows <- function(conn, scr_ids) {
  if (length(scr_ids) == 0) return(0)
  placeholders <- paste(rep("?", length(scr_ids)), collapse = ", ")
  sql <- paste0("DELETE FROM screening_data WHERE scr_id IN (", placeholders, ")")
  DBI::dbExecute(conn, sql, params = as.list(scr_ids))
}

insert_scr_from_import <- function(conn, long_data, screening_type = "") {
  if (is.null(long_data) || nrow(long_data) == 0) return(0)
  batch_id <- paste0("scr_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  n <- nrow(long_data)

  # Принудительно приводим все столбцы к простым атомарным векторам
  insert_df <- data.frame(
    mo_id           = as.integer(long_data$entity_id),
    data_year       = as.integer(long_data$year),
    data_month      = as.integer(long_data$month),
    screening_type  = rep(as.character(screening_type), n),
    indicator       = as.character(long_data$indicator),
    value           = as.numeric(long_data$value),
    import_date     = rep(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), n),
    import_batch_id = rep(batch_id, n),
    stringsAsFactors = FALSE
  )
  insert_df <- insert_df[!is.na(insert_df$mo_id), , drop = FALSE]
  if (nrow(insert_df) == 0) return(0)

  DBI::dbBegin(conn)
  tryCatch({
    DBI::dbAppendTable(conn, "screening_data", insert_df)
    DBI::dbCommit(conn)
    nrow(insert_df)
  }, error = function(e) {
    DBI::dbRollback(conn)
    stop(e)
  })
}

# === CRUD: МО (координаты, название и т.д.) ===

update_mo_field <- function(conn, mo_id, field, value) {
  allowed <- c("mo_name", "mo_short_name", "mo_type", "ownership",
                "latitude", "longitude", "active")
  if (!field %in% allowed) return(FALSE)
  sql <- paste0("UPDATE medical_organizations SET ", field, " = ?, updated_at = CURRENT_TIMESTAMP WHERE mo_id = ?")
  DBI::dbExecute(conn, sql, params = list(value, mo_id))
  TRUE
}

add_mo <- function(conn, mo_name, mo_type, latitude, longitude, district_id) {
  normalized <- tolower(gsub("[^[:alnum:]]", "", mo_name))
  DBI::dbExecute(conn, "
    INSERT INTO medical_organizations (mo_name, mo_short_name, mo_name_normalized,
                                        mo_type, latitude, longitude, active)
    VALUES (?, ?, ?, ?, ?, ?, 1)
  ", params = list(mo_name, mo_name, normalized, mo_type, latitude, longitude))
  # Получаем ID новой МО
  new_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id
  # Связываем с районом
  if (!is.na(district_id) && district_id > 0) {
    DBI::dbExecute(conn, "
      INSERT INTO mo_district_link (mo_id, district_id, is_primary) VALUES (?, ?, 1)
    ", params = list(new_id, district_id))
  }
  new_id
}

# === ЛОГИРОВАНИЕ ИМПОРТА ===

log_import <- function(conn, type, filename, total, imported, failed, status, error_msg = NULL) {
  batch_id <- paste0(type, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  DBI::dbExecute(conn, "
    INSERT INTO import_log (import_batch_id, import_type, file_name,
                            records_total, records_imported, records_failed,
                            status, error_message, import_date)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)
  ", params = list(batch_id, type, filename, total, imported, failed, status, error_msg))
}

# === СОЗДАНИЕ ПУСТЫХ СТРУКТУР ДАННЫХ ===

create_empty_mo <- function() {
  data.frame(
    mo_id            = integer(0),
    mo_name          = character(0),
    mo_short_name    = character(0),
    mo_name_normalized = character(0),
    mo_type          = character(0),
    ownership        = character(0),
    latitude         = numeric(0),
    longitude        = numeric(0),
    district_id      = integer(0),
    district_name_ru = character(0),
    stringsAsFactors = FALSE
  )
}

create_empty_epidemiology <- function() {
  data.frame(
    epi_id      = integer(0),
    district_id = integer(0),
    year        = integer(0),
    month       = integer(0),
    indicator   = character(0),
    value       = numeric(0),
    import_date = character(0),
    stringsAsFactors = FALSE
  )
}

create_empty_screening <- function() {
  data.frame(
    scr_id      = integer(0),
    mo_id       = integer(0),
    year        = integer(0),
    month       = integer(0),
    indicator   = character(0),
    value       = numeric(0),
    import_date = character(0),
    stringsAsFactors = FALSE
  )
}
