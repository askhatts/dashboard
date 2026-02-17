# ============================================================
# ИНИЦИАЛИЗАЦИЯ ДАННЫХ ИЗ ШЕЙПФАЙЛОВ
# Версия: 4.0 (реальные координаты МО, PCODEs районов)
# ============================================================
# Этот скрипт запускается ОДИН РАЗ для создания .rds файлов
# из шейпфайлов и встроенного списка медорганизаций.
# После выполнения в data/ появятся:
#   districts.rds  — sf-объект районов Абайской области (с PCODE)
#   mo.rds         — data.frame МО с реальными GPS координатами
#   epidemiology.rds — пустая таблица эпидемиологии (длинный формат)
#   screening.rds  — пустая таблица скрининга (длинный формат)
# ============================================================

library(sf)
library(dplyr)

# === ФУНКЦИЯ: Нормализация названий МО ===
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

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # --- 1. ЗАГРУЗКА И ФИЛЬТРАЦИЯ ШЕЙПФАЙЛА ---
  cat("1. Загрузка шейпфайла...\n")

  if (!file.exists(shapefile_path)) {
    stop("Шейп-файл не найден: ", shapefile_path, "\n",
         "   Убедитесь, что ВСЕ файлы (.shp, .shx, .dbf, .prj, .cpg)\n",
         "   находятся в папке data/geodata/")
  }

  shp_all <- st_read(shapefile_path, quiet = TRUE)
  cat("   Прочитано", nrow(shp_all), "полигонов Казахстана\n")

  abai_districts <- shp_all %>%
    filter(ADM1_EN == "Abay Region") %>%
    st_transform(4326)

  cat("   Найдено", nrow(abai_districts), "районов Абайской области\n\n")

  if (nrow(abai_districts) == 0) {
    stop("Не найдены районы Абайской области в шейп-файле!")
  }

  # --- 2. МАППИНГ РУССКИХ НАЗВАНИЙ РАЙОНОВ ---
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

    name_ru <- district_names_ru[row$ADM2_EN]
    if (is.na(name_ru)) name_ru <- row$ADM2_EN

    dtype <- ifelse(grepl("District", row$ADM2_EN), "район", "город")

    # PCODE из шейпфайла (стабильный уникальный ID)
    pcode <- if ("ADM2_PCODE" %in% names(row)) as.character(row$ADM2_PCODE) else paste0("KAZ_", sprintf("%03d", i))

    row_metric <- st_transform(row, 32643)
    area_km2 <- as.numeric(st_area(row_metric)) / 1e6

    centroid <- st_centroid(row$geometry)
    coords <- st_coordinates(centroid)

    districts_list[[i]] <- data.frame(
      district_id      = i,
      district_pcode   = pcode,
      district_name_ru = as.character(name_ru),
      district_name_en = as.character(row$ADM2_EN),
      district_type    = dtype,
      area_km2         = round(area_km2, 2),
      centroid_lat     = coords[2],
      centroid_lon     = coords[1],
      stringsAsFactors = FALSE
    )
  }

  districts_df <- do.call(rbind, districts_list)
  districts_sf <- st_sf(
    districts_df,
    geometry = abai_districts$geometry,
    crs = 4326
  )

  cat("   Сформировано", nrow(districts_sf), "районов\n\n")

  # --- 4. ФОРМИРОВАНИЕ СПИСКА МЕДОРГАНИЗАЦИЙ ---
  cat("3. Формирование списка медорганизаций...\n")

  # Встроенный перечень 30 МО с реальными GPS координатами
  # Координаты из открытых данных (2GIS, Yandex Maps)
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
    real_lat = c(
      50.435000,   # ЦПМСП №12
      50.419746,   # Поликлиника №1
      51.122560,   # Поликлиника №2
      50.430385,   # Поликлиника №4
      50.406912,   # Поликлиника №7
      50.391138,   # Поликлиника №9
      50.382218,   # ЦПМСП №10
      50.390807,   # Шульбинская ВА
      50.390489,   # МУ Победа
      50.461134,   # Поликлиника №8
      50.244000,   # Центральная смотровая поликлиника
      50.415431,   # Жан-Ер
      50.428204,   # IV Plus
      50.424690,   # Поликлиника №6
      50.393667,   # Железнодорожная больница
      50.401112,   # Әділ-Ем
      48.943634,   # Абайская районная больница
      49.199488,   # Абралинская больница
      47.766925,   # Аксуатская ЦРБ
      50.873300,   # Бескарагайская ЦРБ
      47.472600,   # Больница района Мақаншы
      47.080264,   # МЦРБ Урджарского района
      50.686882,   # Бородулихинская ЦРБ
      49.336640,   # Жарминская ЦРБ
      49.572845,   # Шарская горбольница
      49.336640,   # Амбулатория Азат (Калбатау)
      48.747806,   # Кокпектинская ЦРБ
      50.452200,   # Курчатовская горбольница
      47.970610,   # МЦРБ Аягозского района
      47.968825    # Казыгул
    ),
    real_lon = c(
      80.275000,   # ЦПМСП №12
      80.256558,   # Поликлиника №1
      71.485110,   # Поликлиника №2
      80.229671,   # Поликлиника №4
      80.241004,   # Поликлиника №7
      80.219088,   # Поликлиника №9
      80.397044,   # ЦПМСП №10
      81.066119,   # Шульбинская ВА
      80.230622,   # МУ Победа
      80.212109,   # Поликлиника №8
      80.133900,   # Центральная смотровая поликлиника
      80.256926,   # Жан-Ер
      80.229018,   # IV Plus
      80.265520,   # Поликлиника №6
      80.259583,   # Железнодорожная больница
      80.298860,   # Әділ-Ем
      79.261491,   # Абайская районная больница
      77.392838,   # Абралинская больница
      82.800734,   # Аксуатская ЦРБ
      79.480311,   # Бескарагайская ЦРБ
      82.010600,   # Больница района Мақаншы
      81.609492,   # МЦРБ Урджарского района
      80.947669,   # Бородулихинская ЦРБ
      81.559450,   # Жарминская ЦРБ
      81.051708,   # Шарская горбольница
      81.559450,   # Амбулатория Азат
      82.381391,   # Кокпектинская ЦРБ
      78.323000,   # Курчатовская горбольница
      80.423637,   # МЦРБ Аягозского района
      80.440427    # Казыгул
    ),
    stringsAsFactors = FALSE
  )

  mo_list <- list()
  for (i in 1:nrow(mo_raw)) {
    d_idx <- which(districts_sf$district_name_en == mo_raw$district_en[i])
    if (length(d_idx) == 0) next

    d <- districts_sf[d_idx, ]

    short <- mo_raw$mo_name[i]
    short <- gsub("КГП на ПХВ |' УЗ ОА|Учреждение |ТОО |Медицинское учреждение ", "", short)
    short <- gsub("'", "", short)

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
      latitude         = mo_raw$real_lat[i],
      longitude        = mo_raw$real_lon[i],
      stringsAsFactors = FALSE
    )
  }

  mo_df <- do.call(rbind, mo_list)
  cat("   Сформировано", nrow(mo_df), "МО с реальными координатами\n\n")

  # --- 5. ПУСТЫЕ ТАБЛИЦЫ (данные загружаются через админку) ---
  cat("4. Создание пустых таблиц данных...\n")

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

  cat("   Таблицы пустые — данные загружаются через админ-панель\n\n")

  # --- 6. СОХРАНЕНИЕ .RDS ФАЙЛОВ ---
  cat("5. Сохранение .rds файлов...\n")

  saveRDS(districts_sf, file.path(output_dir, "districts.rds"))
  saveRDS(mo_df,        file.path(output_dir, "mo.rds"))
  saveRDS(epidemiology, file.path(output_dir, "epidemiology.rds"))
  saveRDS(screening,    file.path(output_dir, "screening.rds"))

  cat("   districts.rds    — ", nrow(districts_sf), "районов (с PCODE)\n")
  cat("   mo.rds           — ", nrow(mo_df), "МО\n")
  cat("   epidemiology.rds — пустая\n")
  cat("   screening.rds    — пустая\n\n")

  cat(strrep("=", 70), "\n")
  cat("  ДАННЫЕ УСПЕШНО ИНИЦИАЛИЗИРОВАНЫ\n")
  cat(strrep("=", 70), "\n\n")

  return(invisible(list(
    districts = districts_sf,
    mo = mo_df,
    epidemiology = epidemiology,
    screening = screening
  )))
}

# === ЗАПУСК ===
if (interactive() || !exists(".setup_sourced_only")) {
  setup_data()
}
