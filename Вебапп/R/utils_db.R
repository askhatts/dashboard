# ============================================================
# УТИЛИТЫ ДЛЯ РАБОТЫ С ДАННЫМИ (.rds файлы)
# ============================================================
# Функции загрузки и сохранения .rds файлов.
# При сохранении создаётся резервная копия (.bak),
# чтобы не потерять данные при сбое записи.
# ============================================================

# === ПУТИ К ФАЙЛАМ ДАННЫХ ===
DATA_DIR       <- "data"
DISTRICTS_PATH <- file.path(DATA_DIR, "districts.rds")
MO_PATH        <- file.path(DATA_DIR, "mo.rds")
EPI_PATH       <- file.path(DATA_DIR, "epidemiology.rds")
SCR_PATH       <- file.path(DATA_DIR, "screening.rds")

# === ЗАГРУЗКА РАЙОНОВ ===
# Возвращает sf-объект с полигонами районов.
# Если файл не найден — возвращает NULL.
load_districts <- function(path = DISTRICTS_PATH) {
  if (file.exists(path)) {
    tryCatch(
      readRDS(path),
      error = function(e) {
        warning("Ошибка чтения districts.rds: ", e$message)
        NULL
      }
    )
  } else {
    warning("Файл не найден: ", path, "\n  Запустите R/setup_database.R для инициализации")
    NULL
  }
}

# === ЗАГРУЗКА МЕДОРГАНИЗАЦИЙ ===
# Возвращает data.frame с координатами и атрибутами МО.
load_mo <- function(path = MO_PATH) {
  if (file.exists(path)) {
    tryCatch(
      readRDS(path),
      error = function(e) {
        warning("Ошибка чтения mo.rds: ", e$message)
        create_empty_mo()
      }
    )
  } else {
    warning("Файл МО не найден: ", path)
    create_empty_mo()
  }
}

# === ЗАГРУЗКА ЭПИДЕМИОЛОГИИ ===
# Возвращает data.frame в длинном формате (indicator + value).
load_epidemiology <- function(path = EPI_PATH) {
  if (file.exists(path)) {
    tryCatch(
      readRDS(path),
      error = function(e) {
        warning("Ошибка чтения epidemiology.rds: ", e$message)
        create_empty_epidemiology()
      }
    )
  } else {
    create_empty_epidemiology()
  }
}

# === ЗАГРУЗКА СКРИНИНГА ===
# Возвращает data.frame в длинном формате (indicator + value).
load_screening <- function(path = SCR_PATH) {
  if (file.exists(path)) {
    tryCatch(
      readRDS(path),
      error = function(e) {
        warning("Ошибка чтения screening.rds: ", e$message)
        create_empty_screening()
      }
    )
  } else {
    create_empty_screening()
  }
}

# === СОХРАНЕНИЕ С РЕЗЕРВНОЙ КОПИЕЙ ===
# Перед перезаписью создаёт .bak копию, чтобы при сбое
# можно было восстановить предыдущую версию данных.
safe_save_rds <- function(data, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)

  # Создаём резервную копию, если файл уже существует
  if (file.exists(path)) {
    bak_path <- paste0(path, ".bak")
    tryCatch(
      file.copy(path, bak_path, overwrite = TRUE),
      error = function(e) warning("Не удалось создать резервную копию: ", e$message)
    )
  }

  # Сохраняем данные
  tryCatch({
    saveRDS(data, path)
    TRUE
  }, error = function(e) {
    warning("Ошибка сохранения ", path, ": ", e$message)
    FALSE
  })
}

# Обёртки для конкретных типов данных
save_districts    <- function(data) safe_save_rds(data, DISTRICTS_PATH)
save_mo           <- function(data) safe_save_rds(data, MO_PATH)
save_epidemiology <- function(data) safe_save_rds(data, EPI_PATH)
save_screening    <- function(data) safe_save_rds(data, SCR_PATH)

# === СОЗДАНИЕ ПУСТЫХ СТРУКТУР ДАННЫХ ===
# Используются, когда .rds файлы ещё не существуют (первый запуск).
# Гарантируют, что приложение не упадёт с ошибкой.

create_empty_mo <- function() {
  data.frame(
    mo_id            = integer(0),
    mo_name          = character(0),
    mo_short_name    = character(0),
    mo_name_normalized = character(0),
    mo_type          = character(0),
    ownership        = character(0),
    district_id      = integer(0),
    district_name_ru = character(0),
    latitude         = numeric(0),
    longitude        = numeric(0),
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
    import_date = as.POSIXct(character(0)),
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
    import_date = as.POSIXct(character(0)),
    stringsAsFactors = FALSE
  )
}
