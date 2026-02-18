# ============================================================
# GLOBAL.R — Загрузка пакетов, констант и начальных данных
# ============================================================
# Этот файл выполняется ОДИН РАЗ при запуске приложения.
# Здесь загружаются все библиотеки, определяются константы
# и читаются данные из SQLite базы.
# ============================================================

# === ЗАГРУЗКА ПАКЕТОВ ===
# Подавляем стартовые сообщения для чистоты лога
suppressPackageStartupMessages({
  library(shiny)          # Основной фреймворк
  library(bslib)          # Bootstrap 5 темизация
  library(leaflet)        # Интерактивные карты
  library(leaflet.extras) # Fullscreen control и доп. возможности
  library(sf)             # Пространственные данные
  library(dplyr)          # Манипуляции с данными
  library(tidyr)          # pivot_longer для конвертации форматов
  library(plotly)         # Интерактивные графики
  library(DT)             # Интерактивные таблицы с редактированием
  library(readxl)         # Чтение Excel-файлов
  library(writexl)        # Запись Excel-файлов (шаблоны)
  library(shinyjs)        # JS-утилиты (show/hide панелей)
  library(shinyWidgets)   # Улучшенные виджеты (pickerInput)
  library(stringdist)     # Нечёткое сопоставление названий МО
  library(htmltools)      # HTML-конструкции для кастомных элементов
  library(digest)         # SHA256-хеш для пароля админки
  library(DBI)            # Интерфейс к базам данных
  library(RSQLite)        # SQLite драйвер
})

# Разрешение конфликтов пакетов (для окружений с пакетом conflicted)
if ("conflicted" %in% loadedNamespaces()) {
  conflicted::conflicts_prefer(graphics::box)
  conflicted::conflicts_prefer(dplyr::filter)
  conflicted::conflicts_prefer(dplyr::lag)
}

# === ПОДКЛЮЧЕНИЕ УТИЛИТ И МОДУЛЕЙ ===
# R Shiny автоматически sourced файлы из R/, но мы делаем это явно
# для контроля порядка загрузки (утилиты ДО модулей)
source("R/utils_db.R")
source("R/utils_spatial.R")
source("R/utils_charts.R")
source("R/utils_import.R")
source("R/mod_controls.R")
source("R/mod_map.R")
source("R/mod_charts.R")
source("R/mod_admin.R")

# === КОНСТАНТЫ ===

# Русские названия месяцев (для фильтров и графиков)
MONTH_NAMES_RU <- c(
  "Январь", "Февраль", "Март", "Апрель", "Май", "Июнь",
  "Июль", "Август", "Сентябрь", "Октябрь", "Ноябрь", "Декабрь"
)

# Центр карты Абайской области (координаты г. Семей)
DEFAULT_MAP_LNG  <- 80.25
DEFAULT_MAP_LAT  <- 50.41
DEFAULT_MAP_ZOOM <- 7

# Пароль администратора (SHA256-хеш строки "admin2024")
# Для изменения пароля: digest::digest("ваш_новый_пароль", algo = "sha256")
ADMIN_PASSWORD_HASH <- digest::digest("admin2024", algo = "sha256")

# === ТЕМА BOOTSTRAP 5 ===
# Определяем тёмную тему в стиле ArcGIS Dashboard.
# Основные цвета:
#   bg (фон)    = #1a1a2e (глубокий тёмно-синий)
#   fg (текст)  = #e0e0e0 (светло-серый)
#   primary     = #00d2ff (голубой акцент)
app_theme <- bs_theme(
  version = 5,
  bg      = "#1a1a2e",
  fg      = "#e0e0e0",
  primary = "#00d2ff",
  secondary = "#6c757d",
  success = "#00e676",
  info    = "#00b0ff",
  warning = "#ffc107",
  danger  = "#ff5252",
  "input-bg"           = "#2b2d42",
  "input-color"        = "#e0e0e0",
  "input-border-color" = "#3a3d5c"
)

# === ЗАГРУЗКА НАЧАЛЬНЫХ ДАННЫХ ИЗ SQLite ===
cat("Загрузка данных из SQLite...\n")

.init_conn <- get_db_connection()
INIT_DISTRICTS    <- load_districts(.init_conn)
INIT_MO           <- load_mo(.init_conn)
INIT_EPIDEMIOLOGY <- load_epidemiology(.init_conn)
INIT_SCREENING    <- load_screening(.init_conn)
DBI::dbDisconnect(.init_conn)
rm(.init_conn)

# Проверяем, есть ли данные
if (is.null(INIT_DISTRICTS)) {
  warning("Районы не загружены! Проверьте SQLite базу data/abai_region.sqlite")
}

cat(sprintf("  Районов: %s | МО: %d | Эпид: %d | Скрин: %d\n",
            ifelse(is.null(INIT_DISTRICTS), "0", as.character(nrow(INIT_DISTRICTS))),
            nrow(INIT_MO),
            nrow(INIT_EPIDEMIOLOGY),
            nrow(INIT_SCREENING)))
cat("Данные загружены.\n\n")
