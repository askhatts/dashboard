# ============================================================
# LAUNCHER - Главное меню приложения
# Версия: 2.1 (исправленная)
# ============================================================

# Определяем рабочую директорию относительно скрипта
# Если запущено из RStudio — используем путь к файлу
# Иначе — текущую рабочую директорию
tryCatch({
  script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
  if (nchar(script_dir) > 0) setwd(script_dir)
}, error = function(e) {
  # Не в RStudio — используем текущую директорию
  message("Используем текущую рабочую директорию: ", getwd())
})

cat("\n")
cat(strrep("=", 60), "\n")
cat("  Система мониторинга скрининга - Абайская область\n")
cat("  Версия 2.1\n")
cat(strrep("=", 60), "\n\n")

# Проверка и установка пакетов
required_packages <- c(
  "shiny", "shinydashboard", "DBI", "RSQLite", "sf",
  "dplyr", "leaflet", "plotly", "DT", "readxl", "stringdist"
)

missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]

if (length(missing_packages) > 0) {
  cat("\u26a0\ufe0f  Обнаружены недостающие пакеты:\n")
  cat("   ", paste(missing_packages, collapse = ", "), "\n\n")
  
  response <- readline(prompt = "Установить их сейчас? (y/n): ")
  
  if (tolower(response) == "y") {
    cat("\nУстановка пакетов...\n")
    install.packages(missing_packages, dependencies = TRUE)
    cat("\n\u2713 Пакеты установлены\n\n")
  } else {
    stop("Установите пакеты вручную: install.packages(c('",
         paste(missing_packages, collapse = "', '"), "'))")
  }
}

# Загрузка необходимых библиотек
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RSQLite)
})

# Проверка структуры папок
dirs_to_create <- c("R", "data", "data/geodata")
for (d in dirs_to_create) {
  if (!dir.exists(d)) {
    cat(sprintf("Создание папки %s/...\n", d))
    dir.create(d, recursive = TRUE)
  }
}

# Проверка наличия базы данных
db_path <- "data/abai_region.sqlite"
db_exists <- file.exists(db_path)

if (db_exists) {
  tryCatch({
    conn <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(conn), add = TRUE)
    
    tables <- dbListTables(conn)
    district_count <- dbGetQuery(conn, "SELECT COUNT(*) as cnt FROM districts")$cnt
    mo_count <- dbGetQuery(conn, "SELECT COUNT(*) as cnt FROM medical_organizations")$cnt
    
    breast_count <- if ("screening_breast_detail" %in% tables) {
      dbGetQuery(conn, "SELECT COUNT(*) as cnt FROM screening_breast_detail")$cnt
    } else 0
    
    cat("\u2713 База данных найдена\n")
    cat(sprintf("  \u2022 Районов: %d\n", district_count))
    cat(sprintf("  \u2022 МО: %d\n", mo_count))
    cat(sprintf("  \u2022 Записей скрининга: %d\n", breast_count))
    
    dbDisconnect(conn)
    on.exit()  # Сбрасываем on.exit после ручного закрытия
  }, error = function(e) {
    cat("\u26a0\ufe0f  База данных повреждена: ", e$message, "\n")
    db_exists <<- FALSE
  })
} else {
  cat("\u26a0\ufe0f  База данных не найдена\n")
  cat("   Выберите опцию 3 для создания БД\n")
}

# Главное меню
cat("\n")
cat(strrep("=", 60), "\n")
cat("  ГЛАВНОЕ МЕНЮ\n")
cat(strrep("=", 60), "\n")
cat("1. Главное приложение (дашборд)\n")
cat("2. Импорт данных\n")
cat("3. Создать/пересоздать базу данных\n")
cat("4. Проверить систему\n")
cat("5. Выход\n")
cat(strrep("=", 60), "\n")

choice <- readline(prompt = "Выберите опцию (1-5): ")

# Обработка выбора
if (choice == "1") {
  cat("\nЗапуск главного приложения...\n")
  
  if (!db_exists) {
    cat("\u274c Сначала создайте базу данных (опция 3)\n")
  } else if (!file.exists("app.R")) {
    cat("\u274c Файл app.R не найден!\n")
  } else {
    tryCatch({
      runApp("app.R", launch.browser = TRUE)
    }, error = function(e) {
      cat("\u274c Ошибка запуска приложения:\n")
      cat("   ", e$message, "\n")
    })
  }
  
} else if (choice == "2") {
  cat("\nЗапуск приложения импорта данных...\n")
  
  if (!db_exists) {
    cat("\u274c Сначала создайте базу данных (опция 3)\n")
  } else if (!file.exists("data_import_app.R")) {
    cat("\u274c Файл data_import_app.R не найден!\n")
  } else {
    tryCatch({
      runApp("data_import_app.R", launch.browser = TRUE)
    }, error = function(e) {
      cat("\u274c Ошибка запуска:\n")
      cat("   ", e$message, "\n")
    })
  }
  
} else if (choice == "3") {
  cat("\n")
  
  if (db_exists) {
    cat("\u26a0\ufe0f  База данных уже существует!\n")
    response <- readline(prompt = "Удалить и пересоздать? ВСЕ ДАННЫЕ БУДУТ ПОТЕРЯНЫ! (yes/no): ")
    
    if (tolower(response) == "yes") {
      cat("\nУдаление старой базы данных...\n")
      file.remove(db_path)
      cat("\u2713 Старая база удалена\n\n")
    } else {
      cat("Отменено\n")
      # НЕ вызываем quit() — просто завершаем скрипт
      return(invisible(NULL))
    }
  }
  
  cat("Создание базы данных...\n\n")
  
  if (!file.exists("R/01_setup_database.R")) {
    cat("\u274c Файл R/01_setup_database.R не найден!\n")
  } else {
    tryCatch({
      source("R/01_setup_database.R", local = TRUE)
      cat("\n\u2713 База данных успешно создана!\n")
    }, error = function(e) {
      cat("\n\u274c Ошибка создания базы данных:\n")
      cat("   ", e$message, "\n\n")
      cat("Проверьте:\n")
      cat("  1. Наличие шейп-файлов в data/geodata/\n")
      cat("  2. Все 5 файлов шейпа (.shp, .shx, .dbf, .prj, .cpg)\n")
    })
  }
  
} else if (choice == "4") {
  cat("\n")
  cat(strrep("=", 60), "\n")
  cat("  ПРОВЕРКА СИСТЕМЫ\n")
  cat(strrep("=", 60), "\n\n")
  
  # Проверка пакетов
  cat("1. Проверка пакетов R:\n")
  all_installed <- TRUE
  for (pkg in required_packages) {
    is_installed <- pkg %in% installed.packages()[, "Package"]
    status <- if (is_installed) "\u2713" else "\u274c"
    version_str <- if (is_installed) as.character(packageVersion(pkg)) else "не установлен"
    cat(sprintf("   %s %s (%s)\n", status, pkg, version_str))
    if (!is_installed) all_installed <- FALSE
  }
  
  # Проверка файлов
  cat("\n2. Проверка файлов проекта:\n")
  files_to_check <- c("app.R", "data_import_app.R", "R/01_setup_database.R", "R/app_functions.R")
  for (f in files_to_check) {
    status <- if (file.exists(f)) "\u2713" else "\u274c"
    cat(sprintf("   %s %s\n", status, f))
  }
  
  # Проверка шейп-файлов
  cat("\n3. Проверка шейп-файлов:\n")
  shp_extensions <- c(".shp", ".shx", ".dbf", ".prj", ".cpg")
  shp_base <- "data/geodata/kaz_admbnda_adm2_unhcr_2023"
  for (ext in shp_extensions) {
    f <- paste0(shp_base, ext)
    status <- if (file.exists(f)) "\u2713" else "\u274c"
    cat(sprintf("   %s %s\n", status, basename(f)))
  }
  
  # Проверка БД
  cat("\n4. Проверка базы данных:\n")
  if (file.exists(db_path)) {
    cat(sprintf("   \u2713 Файл существует (%.2f MB)\n", file.info(db_path)$size / 1024^2))
    tryCatch({
      conn <- dbConnect(SQLite(), db_path)
      tables <- dbListTables(conn)
      cat(sprintf("   \u2713 Таблиц: %d\n", length(tables)))
      
      for (tbl in c("districts", "medical_organizations", "screening_breast_detail",
                     "screening_colorectal_detail", "epidemiology_district")) {
        if (tbl %in% tables) {
          cnt <- dbGetQuery(conn, sprintf("SELECT COUNT(*) as cnt FROM %s", tbl))$cnt
          cat(sprintf("   \u2713 %s: %d записей\n", tbl, cnt))
        }
      }
      dbDisconnect(conn)
    }, error = function(e) {
      cat("   \u274c Ошибка чтения:", e$message, "\n")
    })
  } else {
    cat("   \u274c База данных не создана\n")
  }
  
  cat("\n")
  cat(strrep("=", 60), "\n")
  cat("Проверка завершена\n")
  
} else if (choice == "5") {
  cat("\nДо свидания!\n\n")
  
} else {
  cat("\n\u274c Неверный выбор. Введите число от 1 до 5.\n\n")
}
