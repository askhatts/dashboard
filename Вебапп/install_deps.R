# ============================================================
# Установка зависимостей — Онкологическая аналитика Абайской области
# ============================================================
# Запустить один раз перед первым запуском приложения:
#   source("install_deps.R")
# Затем:
#   shiny::runApp(".")
# ============================================================

required_packages <- c(
  "shiny", "bslib", "leaflet", "leaflet.extras", "sf",
  "dplyr", "tidyr", "plotly", "DT",
  "readxl", "writexl",
  "shinyjs", "shinyWidgets",
  "stringdist", "htmltools", "digest",
  "DBI", "RSQLite"
)

missing <- required_packages[!required_packages %in% installed.packages()[, "Package"]]

if (length(missing) > 0) {
  cat("Установка недостающих пакетов:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, dependencies = TRUE)
  cat("Готово.\n")
} else {
  cat("Все пакеты уже установлены.\n")
}

cat("Запуск приложения: shiny::runApp('.')\n")
