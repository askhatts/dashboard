# ============================================================
# СОЗДАНИЕ БАЗЫ ДАННЫХ
# Версия: 2.1 (исправленная)
# ============================================================

library(DBI)
library(RSQLite)
library(sf)
library(dplyr)

setup_database <- function(
  shapefile_path = "data/geodata/kaz_admbnda_adm2_unhcr_2023.shp",
  db_path = "data/abai_region.sqlite"
) {
  
  dir.create(dirname(db_path), showWarnings = FALSE, recursive = TRUE)
  
  cat("\n", strrep("=", 70), "\n")
  cat("СОЗДАНИЕ БАЗЫ ДАННЫХ\n")
  cat(strrep("=", 70), "\n\n")
  
  # Проверка шейп-файла
  if (!file.exists(shapefile_path)) {
    stop("Шейп-файл не найден: ", shapefile_path, "\n",
         "   Убедитесь, что ВСЕ файлы шейпа (.shp, .shx, .dbf, .prj, .cpg)\n",
         "   находятся в папке data/geodata/")
  }
  
  # === 1. Загрузка шейп-файла ===
  cat("1. Загрузка географических данных...\n")
  shp_all <- st_read(shapefile_path, quiet = TRUE)
  cat("   Прочитано", nrow(shp_all), "районов Казахстана\n\n")
  
  # === 2. Фильтрация Абайской области ===
  cat("2. Фильтрация Абайской области...\n")
  abai_districts <- shp_all %>%
    filter(ADM1_EN == "Abay Region") %>%
    st_transform(4326)
  
  cat("   Найдено", nrow(abai_districts), "районов\n\n")
  
  if (nrow(abai_districts) == 0) {
    stop("Не найдены районы Абайской области в шейп-файле!")
  }
  
  # === 3. Создание структуры БД ===
  cat("3. Создание структуры базы данных...\n")
  
  conn <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  
  # Включаем поддержку внешних ключей
  dbExecute(conn, "PRAGMA foreign_keys = ON")
  dbExecute(conn, "PRAGMA journal_mode = WAL")
  
  # Удаление старых таблиц (порядок важен из-за FK)
  tables_to_drop <- c(
    "import_log", "data_quality_issues",
    "epidemiology_district",
    "screening_breast_aggregated", "screening_colorectal_aggregated",
    "screening_breast_detail", "screening_colorectal_detail",
    "mo_district_link",
    "medical_organizations",
    "districts"
  )
  
  for (tbl in tables_to_drop) {
    dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s", tbl))
  }
  
  # === ТАБЛИЦА 1: РАЙОНЫ ===
  dbExecute(conn, "
    CREATE TABLE districts (
      district_id INTEGER PRIMARY KEY AUTOINCREMENT,
      district_name_en TEXT UNIQUE NOT NULL,
      district_name_ru TEXT NOT NULL,
      district_pcode TEXT,
      district_type TEXT CHECK(district_type IN ('район', 'город')),
      area_km2 REAL,
      population INTEGER,
      geometry_wkt TEXT NOT NULL,
      centroid_lat REAL NOT NULL,
      centroid_lon REAL NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # === ТАБЛИЦА 2: МЕДОРГАНИЗАЦИИ ===
  dbExecute(conn, "
    CREATE TABLE medical_organizations (
      mo_id INTEGER PRIMARY KEY AUTOINCREMENT,
      mo_name TEXT UNIQUE NOT NULL,
      mo_name_normalized TEXT NOT NULL,
      mo_short_name TEXT NOT NULL,
      mo_type TEXT CHECK(mo_type IN ('Больница', 'Поликлиника', 'Амбулатория', 'Частная клиника', 'Другое')),
      ownership TEXT CHECK(ownership IN ('Государственная', 'Частная')),
      latitude REAL,
      longitude REAL,
      custom_coords BOOLEAN DEFAULT 0,
      active BOOLEAN DEFAULT 1,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # === ТАБЛИЦА 3: СВЯЗЬ МО-РАЙОН ===
  dbExecute(conn, "
    CREATE TABLE mo_district_link (
      link_id INTEGER PRIMARY KEY AUTOINCREMENT,
      mo_id INTEGER NOT NULL REFERENCES medical_organizations(mo_id) ON DELETE CASCADE,
      district_id INTEGER NOT NULL REFERENCES districts(district_id) ON DELETE CASCADE,
      is_primary BOOLEAN DEFAULT 1,
      UNIQUE(mo_id, district_id)
    )
  ")
  
  # === ТАБЛИЦА 4: ДЕТАЛЬНЫЕ ДАННЫЕ РМЖ ===
  dbExecute(conn, "
    CREATE TABLE screening_breast_detail (
      record_id INTEGER PRIMARY KEY AUTOINCREMENT,
      mo_id INTEGER NOT NULL REFERENCES medical_organizations(mo_id),
      catchment_area TEXT,
      gender TEXT CHECK(gender IN ('Ж', 'М', NULL)),
      age INTEGER CHECK(age IS NULL OR (age BETWEEN 18 AND 100)),
      start_date DATE,
      start_year INTEGER NOT NULL,
      start_month INTEGER CHECK(start_month IS NULL OR (start_month BETWEEN 1 AND 12)),
      planned_end_date DATE,
      actual_end_date DATE,
      requires_intervention BOOLEAN,
      status_refusal BOOLEAN,
      status_expired BOOLEAN,
      checklist_findings BOOLEAN,
      diagnosis_findings BOOLEAN,
      dispensary_by_detection BOOLEAN,
      dispensary_other BOOLEAN,
      birads_total BOOLEAN,
      birads_m0_tech BOOLEAN,
      birads_m0_diag BOOLEAN,
      birads_m1 BOOLEAN,
      birads_m2 BOOLEAN,
      birads_m3 BOOLEAN,
      birads_m4_m5_mass BOOLEAN,
      birads_m4_m5_asymmetry BOOLEAN,
      birads_m4_m5_distortion BOOLEAN,
      birads_m4_m5_calcifications BOOLEAN,
      benign_neoplasm TEXT,
      breast_cancer TEXT,
      biopsy_performed BOOLEAN,
      import_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      import_batch_id TEXT
    )
  ")
  
  # === ТАБЛИЦА 5: ДЕТАЛЬНЫЕ ДАННЫЕ КРР ===
  dbExecute(conn, "
    CREATE TABLE screening_colorectal_detail (
      record_id INTEGER PRIMARY KEY AUTOINCREMENT,
      mo_id INTEGER NOT NULL REFERENCES medical_organizations(mo_id),
      catchment_area TEXT,
      age INTEGER CHECK(age IS NULL OR (age BETWEEN 18 AND 100)),
      start_date DATE,
      start_year INTEGER NOT NULL,
      start_month INTEGER CHECK(start_month IS NULL OR (start_month BETWEEN 1 AND 12)),
      planned_end_date DATE,
      actual_end_date DATE,
      requires_intervention BOOLEAN,
      status_refusal BOOLEAN,
      status_expired BOOLEAN,
      checklist_findings BOOLEAN,
      diagnosis_findings BOOLEAN,
      dispensary_by_detection BOOLEAN,
      dispensary_other BOOLEAN,
      hemoccult_total BOOLEAN,
      hemoccult_negative BOOLEAN,
      hemoccult_positive BOOLEAN,
      hemoccult_doubtful BOOLEAN,
      colonoscopy_total BOOLEAN,
      boston_score_5_or_less BOOLEAN,
      cs1_no_pathology BOOLEAN,
      cs2_hereditary BOOLEAN,
      cs3_chronic_inflammation BOOLEAN,
      cs4_polyps BOOLEAN,
      cs5_benign_tumors BOOLEAN,
      cs6_malignant_under_1cm_unverified BOOLEAN,
      cs7_malignant_under_1cm_verified BOOLEAN,
      cs8_malignant_over_1cm_unverified BOOLEAN,
      cs9_malignant_over_1cm_verified BOOLEAN,
      biopsy_taken BOOLEAN,
      benign_neoplasm TEXT,
      colorectal_cancer TEXT,
      import_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      import_batch_id TEXT
    )
  ")
  
  # === ТАБЛИЦА 6: АГРЕГАТЫ РМЖ ===
  dbExecute(conn, "
    CREATE TABLE screening_breast_aggregated (
      agg_id INTEGER PRIMARY KEY AUTOINCREMENT,
      mo_id INTEGER NOT NULL REFERENCES medical_organizations(mo_id),
      data_year INTEGER NOT NULL CHECK(data_year BETWEEN 2020 AND 2035),
      data_month INTEGER CHECK(data_month IS NULL OR (data_month BETWEEN 1 AND 12)),
      total_started INTEGER DEFAULT 0,
      total_completed INTEGER DEFAULT 0,
      total_refusals INTEGER DEFAULT 0,
      total_expired INTEGER DEFAULT 0,
      requires_intervention_count INTEGER DEFAULT 0,
      checklist_findings_count INTEGER DEFAULT 0,
      diagnosis_findings_count INTEGER DEFAULT 0,
      dispensary_detection_count INTEGER DEFAULT 0,
      dispensary_other_count INTEGER DEFAULT 0,
      birads_m0_count INTEGER DEFAULT 0,
      birads_m1_count INTEGER DEFAULT 0,
      birads_m2_count INTEGER DEFAULT 0,
      birads_m3_count INTEGER DEFAULT 0,
      birads_m4_m5_count INTEGER DEFAULT 0,
      benign_count INTEGER DEFAULT 0,
      cancer_count INTEGER DEFAULT 0,
      biopsy_count INTEGER DEFAULT 0,
      coverage_percent REAL,
      detection_rate REAL,
      ppv REAL,
      completion_rate REAL,
      refusal_rate REAL,
      last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      UNIQUE(mo_id, data_year, data_month)
    )
  ")
  
  # === ТАБЛИЦА 7: АГРЕГАТЫ КРР ===
  dbExecute(conn, "
    CREATE TABLE screening_colorectal_aggregated (
      agg_id INTEGER PRIMARY KEY AUTOINCREMENT,
      mo_id INTEGER NOT NULL REFERENCES medical_organizations(mo_id),
      data_year INTEGER NOT NULL CHECK(data_year BETWEEN 2020 AND 2035),
      data_month INTEGER CHECK(data_month IS NULL OR (data_month BETWEEN 1 AND 12)),
      total_started INTEGER DEFAULT 0,
      total_completed INTEGER DEFAULT 0,
      total_refusals INTEGER DEFAULT 0,
      total_expired INTEGER DEFAULT 0,
      hemoccult_total INTEGER DEFAULT 0,
      hemoccult_positive INTEGER DEFAULT 0,
      hemoccult_negative INTEGER DEFAULT 0,
      hemoccult_doubtful INTEGER DEFAULT 0,
      hemoccult_positive_percent REAL,
      colonoscopy_total INTEGER DEFAULT 0,
      colonoscopy_with_findings INTEGER DEFAULT 0,
      benign_count INTEGER DEFAULT 0,
      cancer_count INTEGER DEFAULT 0,
      biopsy_count INTEGER DEFAULT 0,
      detection_rate REAL,
      completion_rate REAL,
      colonoscopy_completion_rate REAL,
      last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      UNIQUE(mo_id, data_year, data_month)
    )
  ")
  
  # === ТАБЛИЦА 8: ЭПИДЕМИОЛОГИЯ ===
  dbExecute(conn, "
    CREATE TABLE epidemiology_district (
      epi_id INTEGER PRIMARY KEY AUTOINCREMENT,
      district_id INTEGER NOT NULL REFERENCES districts(district_id),
      data_year INTEGER NOT NULL CHECK(data_year BETWEEN 2020 AND 2035),
      data_month INTEGER CHECK(data_month IS NULL OR (data_month BETWEEN 1 AND 12)),
      cancer_type TEXT NOT NULL CHECK(cancer_type IN ('breast', 'colorectal', 'cervical', 'all')),
      incidence_count INTEGER,
      incidence_rate REAL,
      mortality_count INTEGER,
      mortality_rate REAL,
      mortality_to_incidence_ratio REAL,
      early_stage_count INTEGER,
      early_stage_percent REAL,
      advanced_stage_34_count INTEGER,
      advanced_stage_percent REAL,
      one_year_mortality_count INTEGER,
      one_year_mortality_percent REAL,
      five_year_survival_count INTEGER,
      five_year_survival_percent REAL,
      population INTEGER,
      target_population INTEGER,
      notes TEXT,
      import_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      import_batch_id TEXT,
      UNIQUE(district_id, data_year, data_month, cancer_type)
    )
  ")
  
  # === ТАБЛИЦА 9: ЛОГ ИМПОРТОВ ===
  dbExecute(conn, "
    CREATE TABLE import_log (
      log_id INTEGER PRIMARY KEY AUTOINCREMENT,
      import_batch_id TEXT UNIQUE NOT NULL,
      import_type TEXT CHECK(import_type IN ('screening_breast', 'screening_colorectal', 'epidemiology', 'coordinates')),
      file_name TEXT NOT NULL,
      records_total INTEGER,
      records_imported INTEGER,
      records_failed INTEGER,
      duration_seconds REAL,
      status TEXT CHECK(status IN ('success', 'partial', 'failed')),
      error_message TEXT,
      import_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Индексы
  dbExecute(conn, "CREATE INDEX idx_districts_name_en ON districts(district_name_en)")
  dbExecute(conn, "CREATE INDEX idx_districts_name_ru ON districts(district_name_ru)")
  dbExecute(conn, "CREATE INDEX idx_mo_name ON medical_organizations(mo_name)")
  dbExecute(conn, "CREATE INDEX idx_mo_normalized ON medical_organizations(mo_name_normalized)")
  dbExecute(conn, "CREATE INDEX idx_mo_type ON medical_organizations(mo_type)")
  dbExecute(conn, "CREATE INDEX idx_mo_district_mo ON mo_district_link(mo_id)")
  dbExecute(conn, "CREATE INDEX idx_mo_district_district ON mo_district_link(district_id)")
  dbExecute(conn, "CREATE INDEX idx_breast_mo_year ON screening_breast_detail(mo_id, start_year, start_month)")
  dbExecute(conn, "CREATE INDEX idx_breast_date ON screening_breast_detail(start_date)")
  dbExecute(conn, "CREATE INDEX idx_colorectal_mo_year ON screening_colorectal_detail(mo_id, start_year, start_month)")
  dbExecute(conn, "CREATE INDEX idx_epi_district_year ON epidemiology_district(district_id, data_year)")
  dbExecute(conn, "CREATE INDEX idx_epi_cancer ON epidemiology_district(cancer_type)")
  dbExecute(conn, "CREATE INDEX idx_import_batch ON import_log(import_batch_id)")
  
  cat("   Создано 9 таблиц с индексами\n\n")
  
  # === 4. Загрузка районов ===
  cat("4. Загрузка районов...\n")
  
  district_names_ru <- c(
    "Semey" = "Семей",
    "Abay District" = "Абайский район",
    "Aksuat District" = "Аксуатский район",
    "Ayagoz District" = "Аягозский район",
    "Beskaragay District" = "Бескарагайский район",
    "Borodulikha District" = "Бородулихинский район",
    "Zharma District" = "Жарминский район",
    "Kokpekti District" = "Кокпектинский район",
    "Kurchatov" = "Курчатов",
    "Urzhar District" = "Урджарский район"
  )
  
  # Подготавливаем параметризованный запрос
  insert_district_stmt <- dbSendStatement(conn, "
    INSERT INTO districts (
      district_name_en, district_name_ru, district_pcode,
      district_type, area_km2, geometry_wkt,
      centroid_lat, centroid_lon
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)")
  
  for (i in 1:nrow(abai_districts)) {
    row <- abai_districts[i, ]
    
    district_name_ru <- district_names_ru[row$ADM2_EN]
    if (is.na(district_name_ru)) district_name_ru <- row$ADM2_EN
    
    district_type <- ifelse(grepl("District", row$ADM2_EN), "район", "город")
    geometry_wkt <- st_as_text(row$geometry)
    centroid <- st_centroid(row$geometry)
    coords <- st_coordinates(centroid)
    
    # ИСПРАВЛЕНИЕ: Правильный расчёт площади
    # Пересчитываем в метрическую проекцию для корректной площади
    row_metric <- st_transform(row, 32643)  # UTM зона 43N для Казахстана
    area_km2 <- as.numeric(st_area(row_metric)) / 1e6
    
    dbBind(insert_district_stmt, list(
      row$ADM2_EN, district_name_ru,
      if (is.null(row$ADM2_PCODE)) NA_character_ else row$ADM2_PCODE,
      district_type, area_km2, geometry_wkt,
      coords[2], coords[1]
    ))
  }
  
  dbClearResult(insert_district_stmt)
  cat("   Загружено", nrow(abai_districts), "районов\n\n")
  
  # === 5. Загрузка МО ===
  cat("5. Загрузка медицинских организаций...\n")
  
  mo_data <- data.frame(
    district = c(
      rep("Semey", 16), rep("Abay District", 2), "Aksuat District",
      "Beskaragay District", rep("Urzhar District", 2), "Borodulikha District",
      rep("Zharma District", 3), "Kokpekti District", "Kurchatov",
      rep("Ayagoz District", 2)
    ),
    name = c(
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
  
  normalize_name <- function(name) {
    tolower(gsub("[^[:alnum:]]", "", as.character(name)))
  }
  
  mo_data <- mo_data %>%
    mutate(
      name_normalized = normalize_name(name),
      short_name = gsub("КГП на ПХВ |' УЗ ОА|Учреждение |ТОО |Медицинское учреждение ", "", name),
      short_name = gsub("'", "", short_name),
      type = case_when(
        grepl("Поликлиника|ПМСП|Центр ПМСП", name) ~ "Поликлиника",
        grepl("больница", name, ignore.case = TRUE) ~ "Больница",
        grepl("амбулатория", name, ignore.case = TRUE) ~ "Амбулатория",
        grepl("ТОО|Медицинское учреждение", name) ~ "Частная клиника",
        TRUE ~ "Другое"
      ),
      ownership = ifelse(grepl("КГП|УЗ ОА", name), "Государственная", "Частная")
    )
  
  # ИСПРАВЛЕНИЕ: Разброс координат МО внутри района
  # чтобы маркеры не наложились друг на друга
  set.seed(42)
  
  # Начинаем транзакцию для batch insert
  dbBegin(conn)
  tryCatch({
    for (i in 1:nrow(mo_data)) {
      district_info <- dbGetQuery(conn, 
        "SELECT district_id, centroid_lat, centroid_lon FROM districts WHERE district_name_en = ?",
        params = list(mo_data$district[i])
      )
      
      if (nrow(district_info) == 0) next
      
      # Добавляем случайное смещение, чтобы МО не наложились
      # ~0.01 градуса ≈ 1 км
      lat_offset <- runif(1, -0.015, 0.015)
      lon_offset <- runif(1, -0.015, 0.015)
      mo_lat <- district_info$centroid_lat + lat_offset
      mo_lon <- district_info$centroid_lon + lon_offset
      
      dbExecute(conn, 
        "INSERT INTO medical_organizations 
         (mo_name, mo_name_normalized, mo_short_name, mo_type, ownership, latitude, longitude)
         VALUES (?, ?, ?, ?, ?, ?, ?)",
        params = list(
          mo_data$name[i], mo_data$name_normalized[i], mo_data$short_name[i],
          mo_data$type[i], mo_data$ownership[i], mo_lat, mo_lon
        )
      )
      
      mo_id <- dbGetQuery(conn, 
        "SELECT mo_id FROM medical_organizations WHERE mo_name = ?",
        params = list(mo_data$name[i])
      )$mo_id
      
      if (length(mo_id) > 0) {
        dbExecute(conn, 
          "INSERT INTO mo_district_link (mo_id, district_id) VALUES (?, ?)",
          params = list(mo_id, district_info$district_id)
        )
      }
    }
    dbCommit(conn)
  }, error = function(e) {
    dbRollback(conn)
    stop("Ошибка при загрузке МО: ", e$message)
  })
  
  cat("   Загружено", nrow(mo_data), "МО\n\n")
  
  # === Итоговая сводка ===
  cat(strrep("=", 70), "\n")
  cat("БАЗА ДАННЫХ УСПЕШНО СОЗДАНА\n")
  cat(strrep("=", 70), "\n\n")
  
  cat("Сводка:\n")
  cat(sprintf("  Файл БД: %s\n", db_path))
  cat(sprintf("  Размер: %.2f MB\n", file.info(db_path)$size / 1024^2))
  cat(sprintf("  Районов: %d\n", dbGetQuery(conn, "SELECT COUNT(*) FROM districts")[[1]]))
  cat(sprintf("  МО: %d\n", dbGetQuery(conn, "SELECT COUNT(*) FROM medical_organizations")[[1]]))
  cat(sprintf("  Таблиц: %d\n", length(dbListTables(conn))))
  
  cat("\nТаблицы данных готовы к импорту:\n")
  cat("  screening_breast_detail (РМЖ детально)\n")
  cat("  screening_colorectal_detail (КРР детально)\n")
  cat("  epidemiology_district (эпидемиология)\n")
  
  cat("\nСледующий шаг: Импорт данных\n")
  cat("   source('launcher.R') -> опция 2\n")
  cat(strrep("=", 70), "\n\n")
  
  return(invisible(db_path))
}

# ЗАПУСК
setup_database(
  shapefile_path = "data/geodata/kaz_admbnda_adm2_unhcr_2023.shp"
)
