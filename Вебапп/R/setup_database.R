# ============================================================
# ИНИЦИАЛИЗАЦИЯ ДАННЫХ ИЗ ШЕЙПФАЙЛОВ
# Версия: 3.0 (полная переработка)
# ============================================================
# Этот скрипт запускается ОДИН РАЗ для создания .rds файлов
# из шейпфайлов и встроенного списка медорганизаций.
# После выполнения в data/ появятся:
#   districts.rds  — sf-объект районов Абайской области
#   mo.rds         — data.frame МО с координатами
#   epidemiology.rds — пустая таблица эпидемиологии (длинный формат)
#   screening.rds  — пустая таблица скрининга (длинный формат)
# ============================================================

library(sf)
library(dplyr)

# === ФУНКЦИЯ: Нормализация названий МО ===
# Убираем все небуквенно-цифровые символы и приводим к нижнему регистру.
# Используется для нечёткого сопоставления при импорте Excel.
normalize_name <- function(name) {
  tolower(gsub("[^[:alnum:]]", "", as.character(name)))
}

# === ОСНОВНАЯ ФУНКЦИЯ ИНИЦИАЛИЗАЦИИ ===
setup_data <- function(
  shapefile_path = "data/geodata/kaz_admbnda_adm2_unhcr_2023.shp",
  output_dir = "data"
) {

  cat("\n", strrep("=", 70), "\n")
  cat("  ИНИЦИАЛИЗАЦИЯ ДАННЫХ — Онкологическая аналитика Абайской области\n")
  cat(strrep("=", 70), "\n\n")

  # Создаём выходные директории
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(output_dir, "templates"), showWarnings = FALSE, recursive = TRUE)

  # --- 1. ЗАГРУЗКА И ФИЛЬТРАЦИЯ ШЕЙПФАЙЛА ---
  cat("1. Загрузка шейпфайла...\n")

  if (!file.exists(shapefile_path)) {
    stop("Шейп-файл не найден: ", shapefile_path, "\n",
         "   Убедитесь, что ВСЕ файлы (.shp, .shx, .dbf, .prj, .cpg)\n",
         "   находятся в папке data/geodata/")
  }

  shp_all <- st_read(shapefile_path, quiet = TRUE)
  cat("   Прочитано", nrow(shp_all), "полигонов Казахстана\n")

  # Фильтруем только Абайскую область
  abai_districts <- shp_all %>%
    filter(ADM1_EN == "Abay Region") %>%
    st_transform(4326)  # Приводим к WGS84 для leaflet

  cat("   Найдено", nrow(abai_districts), "районов Абайской области\n\n")

  if (nrow(abai_districts) == 0) {
    stop("Не найдены районы Абайской области в шейп-файле!")
  }

  # --- 2. МАППИНГ РУССКИХ НАЗВАНИЙ РАЙОНОВ ---
  # Словарь перевода названий из шейпфайла (англ.) → русские
  district_names_ru <- c(
    "Semey"               = "Семей",
    "Abay District"       = "Абайский район",
    "Aksuat District"     = "Аксуатский район",
    "Ayagoz District"     = "Аягозский район",
    "Beskaragay District" = "Бескарагайский район",
    "Borodulikha District" = "Бородулихинский район",
    "Zharma District"     = "Жарминский район",
    "Kokpekti District"   = "Кокпектинский район",
    "Kurchatov"           = "Курчатов",
    "Urzhar District"     = "Урджарский район"
  )

  # --- 3. ФОРМИРОВАНИЕ SF-ОБЪЕКТА РАЙОНОВ ---
  cat("2. Формирование данных районов...\n")

  districts_list <- list()

  for (i in 1:nrow(abai_districts)) {
    row <- abai_districts[i, ]

    # Русское название (из маппинга или оригинальное, если не найдено)
    name_ru <- district_names_ru[row$ADM2_EN]
    if (is.na(name_ru)) name_ru <- row$ADM2_EN

    # Тип: район или город
    dtype <- ifelse(grepl("District", row$ADM2_EN), "район", "город")

    # Площадь в км2 — пересчитываем в метрическую проекцию UTM 43N
    row_metric <- st_transform(row, 32643)
    area_km2 <- as.numeric(st_area(row_metric)) / 1e6

    # Центроид (для начальных координат МО)
    centroid <- st_centroid(row$geometry)
    coords <- st_coordinates(centroid)

    districts_list[[i]] <- data.frame(
      district_id    = i,
      district_name_ru = as.character(name_ru),
      district_name_en = as.character(row$ADM2_EN),
      district_type  = dtype,
      area_km2       = round(area_km2, 2),
      centroid_lat   = coords[2],
      centroid_lon   = coords[1],
      stringsAsFactors = FALSE
    )
  }

  # Собираем data.frame и присоединяем геометрию
  districts_df <- do.call(rbind, districts_list)
  districts_sf <- st_sf(
    districts_df,
    geometry = abai_districts$geometry,
    crs = 4326
  )

  cat("   Сформировано", nrow(districts_sf), "районов\n\n")

  # --- 4. ФОРМИРОВАНИЕ СПИСКА МЕДОРГАНИЗАЦИЙ ---
  cat("3. Формирование списка медорганизаций...\n")

  # Встроенный перечень 30 МО Абайской области с привязкой к районам (англ. название)
  mo_raw <- data.frame(
    district_en = c(
      rep("Semey", 16),
      rep("Abay District", 2),
      "Aksuat District",
      "Beskaragay District",
      rep("Urzhar District", 2),
      "Borodulikha District",
      rep("Zharma District", 3),
      "Kokpekti District",
      "Kurchatov",
      rep("Ayagoz District", 2)
    ),
    mo_name = c(
      "КГП на ПХВ 'Центр ПМСП №12 г. Семей' УЗ ОА",
      "КГП на ПХВ 'Поликлиника №1 г. Семей' УЗ ОА",
      "КГП на ПХВ 'Поликлиника №2 г. Семей' УЗ ОА",
      "КГП на ПХВ 'Поликлиника №4 г. Семей' УЗ ОА",
      "КГП на ПХВ 'Поликлиника №7 г. Семей' УЗ ОА",
      "КГП на ПХВ 'Поликлиника №9 города Семей' УЗ ОА",
      "КГП на ПХВ 'Центр ПМСП №10 города Семей' УЗ ОА",
      "КГП на ПХВ 'Шульбинская врачебная амбулатория' УЗ ОА",
      "Медицинское учреждение 'Победа'",
      "Медицинское учреждение 'Поликлиника №8 города Семей'",
      "Медицинское учреждение 'Центральная смотровая поликлиника'",
      "ТОО 'ЖАН-ЕР' г. Семей",
      "ТОО 'Клиника iv plus'",
      "ТОО 'Поликлиника №6 города Семей'",
      "ТОО 'Семейская железнодорожная больница'",
      "ТОО 'Әділ-Ем'",
      "КГП на ПХВ 'Абайская районная больница' УЗ ОА",
      "КГП на ПХВ 'Абралинская больница' УЗ ОА",
      "КГП на ПХВ 'Аксуатская районная больница' УЗ ОА",
      "КГП на ПХВ 'Бескарагайская районная больница' УЗ ОА",
      "КГП на ПХВ 'Больница района Мақаншы' УЗ ОА",
      "КГП на ПХВ 'МЦРБ Урджарского района' УЗ ОА",
      "КГП на ПХВ 'Бородулихинская районная больница' УЗ ОА",
      "КГП на ПХВ 'Жарминская районная больница' УЗ ОА",
      "КГП на ПХВ 'Шарская городская больница' УЗ ОА",
      "Учреждение 'Семейная амбулатория Азат' (с. Калбатау)",
      "КГП на ПХВ 'Кокпектинская районная больница' УЗ ОА",
      "КГП на ПХВ 'Курчатовская городская больница' УЗ ОА",
      "КГП на ПХВ 'МЦРБ Аягозского района' УЗ ОА",
      "Учреждение 'Казыгул'"
    ),
    stringsAsFactors = FALSE
  )

  # Формируем data.frame МО с координатами
  set.seed(42)  # Фиксируем seed для воспроизводимости разброса координат

  mo_list <- list()
  for (i in 1:nrow(mo_raw)) {
    # Находим район по английскому названию
    d_idx <- which(districts_sf$district_name_en == mo_raw$district_en[i])
    if (length(d_idx) == 0) next

    d <- districts_sf[d_idx, ]

    # Координаты: центроид района + случайный разброс (~1 км = 0.01 градуса)
    # чтобы маркеры МО не наложились друг на друга
    lat_offset <- runif(1, -0.015, 0.015)
    lon_offset <- runif(1, -0.015, 0.015)

    # Короткое название: убираем типовые обёртки
    short <- mo_raw$mo_name[i]
    short <- gsub("КГП на ПХВ |' УЗ ОА|Учреждение |ТОО |Медицинское учреждение ", "", short)
    short <- gsub("'", "", short)

    # Определяем тип и форму собственности по ключевым словам в названии
    mo_type <- case_when(
      grepl("Поликлиника|ПМСП|Центр ПМСП", mo_raw$mo_name[i]) ~ "Поликлиника",
      grepl("больница", mo_raw$mo_name[i], ignore.case = TRUE)  ~ "Больница",
      grepl("амбулатория", mo_raw$mo_name[i], ignore.case = TRUE) ~ "Амбулатория",
      grepl("ТОО|Медицинское учреждение", mo_raw$mo_name[i])    ~ "Частная клиника",
      TRUE ~ "Другое"
    )

    ownership <- ifelse(grepl("КГП|УЗ ОА", mo_raw$mo_name[i]), "Государственная", "Частная")

    mo_list[[i]] <- data.frame(
      mo_id            = i,
      mo_name          = mo_raw$mo_name[i],
      mo_short_name    = short,
      mo_name_normalized = normalize_name(mo_raw$mo_name[i]),
      mo_type          = mo_type,
      ownership        = ownership,
      district_id      = d$district_id,
      district_name_ru = d$district_name_ru,
      latitude         = d$centroid_lat + lat_offset,
      longitude        = d$centroid_lon + lon_offset,
      stringsAsFactors = FALSE
    )
  }

  mo_df <- do.call(rbind, mo_list)
  cat("   Сформировано", nrow(mo_df), "МО\n\n")

  # --- 5. СОЗДАНИЕ ПУСТЫХ ТАБЛИЦ ДАННЫХ (длинный формат) ---
  cat("4. Создание пустых таблиц данных...\n")

  # Эпидемиология — длинный формат
  # Каждая строка = один показатель для одного района за один период
  epidemiology <- data.frame(
    epi_id      = integer(0),
    district_id = integer(0),
    year        = integer(0),
    month       = integer(0),
    indicator   = character(0),
    value       = numeric(0),
    import_date = as.POSIXct(character(0)),
    stringsAsFactors = FALSE
  )

  # Скрининг — длинный формат
  # Каждая строка = один показатель для одной МО за один период
  screening <- data.frame(
    scr_id      = integer(0),
    mo_id       = integer(0),
    year        = integer(0),
    month       = integer(0),
    indicator   = character(0),
    value       = numeric(0),
    import_date = as.POSIXct(character(0)),
    stringsAsFactors = FALSE
  )

  # --- 6. ГЕНЕРАЦИЯ ДЕМО-ДАННЫХ ---
  # Создаём небольшой набор демонстрационных данных,
  # чтобы дашборд не был пустым при первом запуске
  cat("5. Генерация демонстрационных данных...\n")

  set.seed(123)
  demo_epi <- list()
  epi_counter <- 1

  # Показатели эпидемиологии
  epi_indicators <- c(
    "Заболеваемость", "Смертность", "Ранняя диагностика (%)",
    "Запущенность (%)", "5-лет. выживаемость (%)"
  )

  for (d_id in districts_sf$district_id) {
    for (yr in 2022:2024) {
      for (ind in epi_indicators) {
        # Генерируем реалистичные значения для каждого показателя
        val <- switch(ind,
          "Заболеваемость"         = round(runif(1, 20, 120), 1),
          "Смертность"             = round(runif(1, 5, 50), 1),
          "Ранняя диагностика (%)" = round(runif(1, 35, 75), 1),
          "Запущенность (%)"       = round(runif(1, 10, 45), 1),
          "5-лет. выживаемость (%)" = round(runif(1, 40, 80), 1),
          round(runif(1, 0, 100), 1)
        )

        demo_epi[[epi_counter]] <- data.frame(
          epi_id      = epi_counter,
          district_id = d_id,
          year        = yr,
          month       = NA_integer_,
          indicator   = ind,
          value       = val,
          import_date = Sys.time(),
          stringsAsFactors = FALSE
        )
        epi_counter <- epi_counter + 1
      }
    }
  }

  epidemiology <- do.call(rbind, demo_epi)
  cat("   Создано", nrow(epidemiology), "записей эпидемиологии\n")

  # Демо-данные скрининга
  demo_scr <- list()
  scr_counter <- 1

  scr_indicators <- c(
    "РМЖ - Начато", "РМЖ - Завершено", "РМЖ - Рак выявлен",
    "КРР - Начато", "КРР - Завершено", "КРР - Рак выявлен",
    "РШМ - Начато", "РШМ - Завершено", "РШМ - Рак выявлен"
  )

  for (m_id in mo_df$mo_id) {
    for (yr in 2022:2024) {
      for (mn in 1:12) {
        for (ind in scr_indicators) {
          # Генерируем значения в зависимости от типа показателя
          val <- if (grepl("Начато", ind)) {
            round(runif(1, 30, 250))
          } else if (grepl("Завершено", ind)) {
            round(runif(1, 25, 220))
          } else {
            round(runif(1, 0, 8))
          }

          demo_scr[[scr_counter]] <- data.frame(
            scr_id      = scr_counter,
            mo_id       = m_id,
            year        = yr,
            month       = mn,
            indicator   = ind,
            value       = val,
            import_date = Sys.time(),
            stringsAsFactors = FALSE
          )
          scr_counter <- scr_counter + 1
        }
      }
    }
  }

  screening <- do.call(rbind, demo_scr)
  cat("   Создано", nrow(screening), "записей скрининга\n\n")

  # --- 7. СОХРАНЕНИЕ .RDS ФАЙЛОВ ---
  cat("6. Сохранение .rds файлов...\n")

  saveRDS(districts_sf, file.path(output_dir, "districts.rds"))
  saveRDS(mo_df,        file.path(output_dir, "mo.rds"))
  saveRDS(epidemiology, file.path(output_dir, "epidemiology.rds"))
  saveRDS(screening,    file.path(output_dir, "screening.rds"))

  cat("   districts.rds    — ", nrow(districts_sf), "районов\n")
  cat("   mo.rds           — ", nrow(mo_df), "МО\n")
  cat("   epidemiology.rds — ", nrow(epidemiology), "записей\n")
  cat("   screening.rds    — ", nrow(screening), "записей\n\n")

  # --- 8. ИТОГОВАЯ СВОДКА ---
  cat(strrep("=", 70), "\n")
  cat("  ДАННЫЕ УСПЕШНО ИНИЦИАЛИЗИРОВАНЫ\n")
  cat(strrep("=", 70), "\n\n")
  cat("  Файлы сохранены в: ", normalizePath(output_dir), "\n")
  cat("  Следующий шаг: запустите приложение — shiny::runApp('.')\n")
  cat(strrep("=", 70), "\n\n")

  return(invisible(list(
    districts = districts_sf,
    mo = mo_df,
    epidemiology = epidemiology,
    screening = screening
  )))
}

# === ЗАПУСК ===
# При прямом выполнении этого файла создаются все .rds
if (interactive() || !exists(".setup_sourced_only")) {
  setup_data()
}
