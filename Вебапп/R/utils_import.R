# ============================================================
# УТИЛИТЫ ИМПОРТА ДАННЫХ ИЗ EXCEL
# ============================================================
# Функции для:
#   1) Нечёткого сопоставления названий МО (Jaro-Winkler)
#   2) Автодетекции колонок Excel
#   3) Конвертации широкого формата Excel в длинный формат
#   4) Парсинг пациентского формата скрининга (per-patient)
#   5) Валидации импортируемых данных
# ============================================================

# === НОРМАЛИЗАЦИЯ НАЗВАНИЙ ===
normalize_name <- function(name) {
  tolower(gsub("[^[:alnum:]]", "", as.character(name)))
}

# === СОПОСТАВЛЕНИЕ НАЗВАНИЙ МО ===
match_mo_names <- function(excel_names, db_mo) {
  results <- data.frame(
    excel_name     = excel_names,
    matched_mo_id  = NA_integer_,
    matched_mo_name = NA_character_,
    confidence     = NA_real_,
    match_method   = NA_character_,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(excel_names)) {
    if (is.na(excel_names[i]) || trimws(excel_names[i]) == "") next

    excel_norm <- normalize_name(excel_names[i])
    if (nchar(excel_norm) == 0) next

    # --- Метод 1: Точное совпадение (нормализованных строк) ---
    exact <- which(db_mo$mo_name_normalized == excel_norm)
    if (length(exact) > 0) {
      results$matched_mo_id[i]  <- db_mo$mo_id[exact[1]]
      results$matched_mo_name[i] <- db_mo$mo_name[exact[1]]
      results$confidence[i]     <- 100
      results$match_method[i]   <- "exact"
      next
    }

    # --- Метод 2: Нечёткое совпадение (Jaro-Winkler, порог 70%) ---
    distances <- stringdist::stringdist(
      excel_norm, db_mo$mo_name_normalized, method = "jw", p = 0.1
    )
    best_idx   <- which.min(distances)
    similarity <- round((1 - distances[best_idx]) * 100)

    if (similarity >= 70) {
      results$matched_mo_id[i]  <- db_mo$mo_id[best_idx]
      results$matched_mo_name[i] <- db_mo$mo_name[best_idx]
      results$confidence[i]     <- similarity
      results$match_method[i]   <- "fuzzy"
      next
    }

    # --- Метод 3: По номеру в названии ---
    excel_num <- regmatches(excel_names[i], regexpr("\\d+", excel_names[i]))
    if (length(excel_num) > 0 && nchar(excel_num) > 0) {
      db_nums <- regmatches(db_mo$mo_name, regexpr("\\d+", db_mo$mo_name))
      num_matches <- which(db_nums == excel_num)

      if (length(num_matches) == 1) {
        results$matched_mo_id[i]  <- db_mo$mo_id[num_matches]
        results$matched_mo_name[i] <- db_mo$mo_name[num_matches]
        results$confidence[i]     <- 80
        results$match_method[i]   <- "number"
      } else if (length(num_matches) > 1) {
        sub_dist <- stringdist::stringdist(
          excel_norm, db_mo$mo_name_normalized[num_matches], method = "jw"
        )
        sub_best <- which.min(sub_dist)
        results$matched_mo_id[i]  <- db_mo$mo_id[num_matches[sub_best]]
        results$matched_mo_name[i] <- db_mo$mo_name[num_matches[sub_best]]
        results$confidence[i]     <- round((1 - sub_dist[sub_best]) * 100)
        results$match_method[i]   <- "number_fuzzy"
      }
    }
  }

  return(results)
}

# === АВТОДЕТЕКЦИЯ КОЛОНОК EXCEL ===
auto_detect_columns <- function(col_names, entity_type = "district") {
  result <- list()
  cn <- tolower(col_names)

  # Колонка-идентификатор сущности
  if (entity_type == "district") {
    id_patterns <- "район|территор|district|id_район|регион"
  } else {
    id_patterns <- "мо|организац|учрежден|клиник|больниц|поликлиник|mo_name|id_мо|начавш"
  }

  id_idx <- which(grepl(id_patterns, cn))
  if (length(id_idx) > 0) result$entity_col <- col_names[id_idx[1]]

  # Прямой ID (mo_id или district_id — в зависимости от entity_type)
  if (entity_type == "mo") {
    mo_id_idx <- which(grepl("^mo_id$|^id_мо$|^мо_id$", cn))
    if (length(mo_id_idx) > 0) result$entity_id_col <- col_names[mo_id_idx[1]]
  } else {
    district_id_idx <- which(grepl("^district_id$|^id_район$|^район_id$", cn))
    if (length(district_id_idx) > 0) result$entity_id_col <- col_names[district_id_idx[1]]
  }

  pcode_idx <- which(grepl("pcode|код_район|adm2_pcode", cn))
  if (length(pcode_idx) > 0) result$pcode_col <- col_names[pcode_idx[1]]

  # Год
  year_idx <- which(grepl("^год$|^year$|^data_year$", cn))
  if (length(year_idx) > 0) result$year_col <- col_names[year_idx[1]]

  # Месяц
  month_idx <- which(grepl("^месяц$|^month$|^data_month$", cn))
  if (length(month_idx) > 0) result$month_col <- col_names[month_idx[1]]

  # Дата (комбинированная)
  date_idx <- which(grepl("^дата$|^date$|дата_начала|start_date", cn))
  if (length(date_idx) > 0) result$date_col <- col_names[date_idx[1]]

  # Координаты
  lat_idx <- which(grepl("^широта$|^latitude$|^lat$|^x$", cn))
  if (length(lat_idx) > 0) result$lat_col <- col_names[lat_idx[1]]

  lon_idx <- which(grepl("^долгота$|^longitude$|^lon$|^lng$|^y$", cn))
  if (length(lon_idx) > 0) result$lon_col <- col_names[lon_idx[1]]

  return(result)
}

# === ОПРЕДЕЛЕНИЕ ФОРМАТА: ПАЦИЕНТСКИЙ ИЛИ АГРЕГИРОВАННЫЙ ===
is_patient_level_format <- function(col_names) {
  cn <- tolower(col_names)
  patient_markers <- c("фио", "иин", "iin", "пол$", "^пол$", "возраст")
  any(sapply(patient_markers, function(p) any(grepl(p, cn))))
}

# === НОРМАЛИЗАЦИЯ НАЗВАНИЙ МО ИЗ EXCEL (с подчёркиваниями) ===
# Заменяет подчёркивания на пробелы, убирает лишние пробелы
normalize_mo_excel_name <- function(name) {
  n <- gsub("_", " ", as.character(name))
  n <- gsub("\\s+", " ", trimws(n))
  n
}

# === ПАРСИНГ ПАЦИЕНТСКОГО ФОРМАТА СКРИНИНГА ===
# Принимает per-patient Excel (каждая строка = один пациент),
# агрегирует по МО + год + месяц.
# Показатели:
#   - Всего_случаев (count per MO)
#   - Медиана_возраст (если есть колонка "Возраст")
#   - Медиана_дней_до_завершения (Дата_окончания - Дата_начала)
#   - Доля_просроченных_% (Дата_окончания > Планируемая_дата_завершения)
#   - Все остальные числовые колонки — SUM
parse_patient_level_screening <- function(data, reference_mo,
                                           scr_type_prefix = "",
                                           year_override = NULL,
                                           month_override = NULL) {
  if (is.null(data) || nrow(data) == 0) stop("Входные данные пусты")

  cn <- tolower(names(data))
  orig_names <- names(data)

  # Шаг 1: Определяем колонку МО
  mo_col_idx <- which(grepl("мо_начавш|мо_начав|^мо$|организац|начавш", cn))
  if (length(mo_col_idx) == 0) {
    text_cols <- which(sapply(data, is.character) | sapply(data, is.factor))
    if (length(text_cols) > 0) {
      mo_col_idx <- text_cols[1]
    } else {
      stop("Не найдена колонка МО")
    }
  }
  mo_col <- orig_names[mo_col_idx[1]]

  # Шаг 2: Определяем колонки дат
  date_start_idx <- which(grepl("дата_начала|дата.*начал|start_date|^дата$", cn))
  date_start_col <- if (length(date_start_idx) > 0) orig_names[date_start_idx[1]] else NULL

  date_end_idx <- which(grepl("дата_окончания|дата.*оконч|end_date|дата_конца", cn))
  date_end_col <- if (length(date_end_idx) > 0) orig_names[date_end_idx[1]] else NULL

  date_planned_idx <- which(grepl("планируем.*дат|планир.*заверш|planned.*date", cn))
  date_planned_col <- if (length(date_planned_idx) > 0) orig_names[date_planned_idx[1]] else NULL

  # Шаг 3: Определяем колонку возраста
  age_col_idx <- which(grepl("^возраст$|^age$", cn))
  age_col <- if (length(age_col_idx) > 0) orig_names[age_col_idx[1]] else NULL

  # Шаг 4: Определяем колонку mo_id
  mo_id_col_idx <- which(grepl("^mo_id$|^id_мо$|^мо_id$", cn))
  mo_id_col <- if (length(mo_id_col_idx) > 0) orig_names[mo_id_col_idx[1]] else NULL

  # Шаг 5: Мета-колонки (не агрегируются как SUM)
  metadata_patterns <- paste0(
    "фио|иин|iin|^пол$|пол$|возраст|age|gender|участок|дата|планируем|",
    "фамил|имя|отчеств|name|area|прикрепл|статус"
  )
  metadata_idx <- which(grepl(metadata_patterns, cn))
  metadata_cols <- orig_names[metadata_idx]
  metadata_cols <- union(metadata_cols, mo_col)
  if (!is.null(date_start_col)) metadata_cols <- union(metadata_cols, date_start_col)
  if (!is.null(date_end_col)) metadata_cols <- union(metadata_cols, date_end_col)
  if (!is.null(date_planned_col)) metadata_cols <- union(metadata_cols, date_planned_col)
  if (!is.null(mo_id_col)) metadata_cols <- union(metadata_cols, mo_id_col)
  if (!is.null(age_col)) metadata_cols <- union(metadata_cols, age_col)

  # Шаг 6: Оставшиеся числовые колонки → SUM-показатели
  potential_indicator_cols <- setdiff(orig_names, metadata_cols)
  for (col in potential_indicator_cols) {
    vals <- suppressWarnings(as.numeric(data[[col]]))
    if (sum(!is.na(vals)) > 0) {
      data[[col]] <- vals
    }
  }
  indicator_cols <- potential_indicator_cols[
    sapply(data[potential_indicator_cols], function(x) is.numeric(x) || is.integer(x))
  ]
  for (col in indicator_cols) {
    data[[col]][is.na(data[[col]])] <- 0
  }

  # Шаг 7: Парсинг дат
  if (!is.null(date_start_col)) {
    data$`.date_start` <- tryCatch(as.Date(data[[date_start_col]]), error = function(e) rep(NA, nrow(data)))
  }
  if (!is.null(date_end_col)) {
    data$`.date_end` <- tryCatch(as.Date(data[[date_end_col]]), error = function(e) rep(NA, nrow(data)))
  }
  if (!is.null(date_planned_col)) {
    data$`.date_planned` <- tryCatch(as.Date(data[[date_planned_col]]), error = function(e) rep(NA, nrow(data)))
  }

  # Шаг 8: Извлекаем год/месяц
  if (!is.null(date_start_col) && ".date_start" %in% names(data)) {
    data$`.year` <- as.integer(format(data$`.date_start`, "%Y"))
    data$`.month` <- as.integer(format(data$`.date_start`, "%m"))
    data$`.year`[is.na(data$`.year`)] <- year_override %||% as.integer(format(Sys.Date(), "%Y"))
    data$`.month`[is.na(data$`.month`)] <- month_override %||% NA_integer_
  } else {
    data$`.year` <- rep(year_override %||% as.integer(format(Sys.Date(), "%Y")), nrow(data))
    data$`.month` <- rep(month_override %||% NA_integer_, nrow(data))
  }

  # Шаг 9: Сопоставление МО → mo_id (с нормализацией подчёркиваний)
  if (!is.null(mo_id_col)) {
    data$`.mo_id` <- suppressWarnings(as.integer(data[[mo_id_col]]))
  } else {
    # Нормализуем имена МО: "_" → " "
    raw_mo_names <- as.character(data[[mo_col]])
    clean_mo_names <- sapply(raw_mo_names, normalize_mo_excel_name, USE.NAMES = FALSE)
    unique_clean <- unique(clean_mo_names)
    mo_matches <- match_mo_names(unique_clean, reference_mo)
    lookup <- setNames(mo_matches$matched_mo_id, unique_clean)
    data$`.mo_id` <- lookup[clean_mo_names]
  }

  # Шаг 10: Фильтруем строки без МО
  data <- data[!is.na(data$`.mo_id`), , drop = FALSE]
  if (nrow(data) == 0) stop("Не удалось сопоставить ни одну МО")

  # Шаг 11: Агрегируем по МО + год + месяц
  results <- list()

  # 11a: Всего случаев (count)
  count_data <- data %>%
    dplyr::group_by(`.mo_id`, `.year`, `.month`) %>%
    dplyr::summarise(value = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(indicator = "Всего_случаев")
  results[[length(results) + 1]] <- count_data

  # 11b: Медиана возраста
  if (!is.null(age_col)) {
    age_vals <- suppressWarnings(as.numeric(data[[age_col]]))
    data$`.age` <- age_vals
    age_data <- data %>%
      dplyr::filter(!is.na(`.age`)) %>%
      dplyr::group_by(`.mo_id`, `.year`, `.month`) %>%
      dplyr::summarise(value = median(`.age`, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(indicator = "Медиана_возраст")
    results[[length(results) + 1]] <- age_data
  }

  # 11c: Медиана дней до завершения (Дата_окончания - Дата_начала)
  if (".date_start" %in% names(data) && ".date_end" %in% names(data)) {
    data$`.days_to_end` <- as.numeric(difftime(data$`.date_end`, data$`.date_start`, units = "days"))
    days_data <- data %>%
      dplyr::filter(!is.na(`.days_to_end`) & `.days_to_end` >= 0) %>%
      dplyr::group_by(`.mo_id`, `.year`, `.month`) %>%
      dplyr::summarise(value = median(`.days_to_end`, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(indicator = "Медиана_дней_до_завершения")
    results[[length(results) + 1]] <- days_data
  }

  # 11d: Доля просроченных (Дата_окончания > Планируемая_дата_завершения)
  if (".date_end" %in% names(data) && ".date_planned" %in% names(data)) {
    delay_data <- data %>%
      dplyr::filter(!is.na(`.date_end`) & !is.na(`.date_planned`)) %>%
      dplyr::group_by(`.mo_id`, `.year`, `.month`) %>%
      dplyr::summarise(
        value = round(sum(`.date_end` > `.date_planned`, na.rm = TRUE) / dplyr::n() * 100, 1),
        .groups = "drop"
      ) %>%
      dplyr::mutate(indicator = "Доля_просроченных_%")
    results[[length(results) + 1]] <- delay_data
  }

  # 11e: SUM-показатели (остальные числовые колонки)
  if (length(indicator_cols) > 0) {
    sum_agg <- data %>%
      dplyr::group_by(`.mo_id`, `.year`, `.month`) %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(indicator_cols), ~sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )
    sum_long <- sum_agg %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(indicator_cols),
        names_to = "indicator",
        values_to = "value"
      ) %>%
      dplyr::filter(!is.na(value) & value != 0)
    results[[length(results) + 1]] <- sum_long
  }

  # Шаг 12: Объединяем все результаты
  long_data <- dplyr::bind_rows(results) %>%
    dplyr::rename(entity_id = `.mo_id`, year = `.year`, month = `.month`) %>%
    dplyr::mutate(import_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

  # Добавляем префикс типа скрининга
  if (nchar(scr_type_prefix) > 0) {
    long_data$indicator <- paste0(scr_type_prefix, " - ", long_data$indicator)
  }

  return(long_data)
}

# === КОНВЕРТАЦИЯ EXCEL (ШИРОКИЙ ФОРМАТ) В ДЛИННЫЙ ФОРМАТ ===
parse_excel_to_long_format <- function(data, entity_type = "district",
                                        reference_data = NULL,
                                        year_override = NULL,
                                        month_override = NULL) {
  if (is.null(data) || nrow(data) == 0) {
    stop("Входные данные пусты")
  }

  detected <- auto_detect_columns(names(data), entity_type)

  service_cols <- c()

  entity_col <- detected$entity_col
  if (is.null(entity_col)) {
    text_cols <- names(data)[sapply(data, is.character) | sapply(data, is.factor)]
    if (length(text_cols) > 0) {
      entity_col <- text_cols[1]
    } else {
      stop("Не удалось определить колонку с названием сущности (район/МО)")
    }
  }
  service_cols <- c(service_cols, entity_col)

  year_col  <- detected$year_col
  month_col <- detected$month_col
  date_col  <- detected$date_col
  lat_col   <- detected$lat_col
  lon_col   <- detected$lon_col
  entity_id_col <- detected$entity_id_col
  pcode_col <- detected$pcode_col

  if (!is.null(year_col))      service_cols <- c(service_cols, year_col)
  if (!is.null(month_col))     service_cols <- c(service_cols, month_col)
  if (!is.null(date_col))      service_cols <- c(service_cols, date_col)
  if (!is.null(lat_col))       service_cols <- c(service_cols, lat_col)
  if (!is.null(lon_col))       service_cols <- c(service_cols, lon_col)
  if (!is.null(entity_id_col)) service_cols <- c(service_cols, entity_id_col)
  if (!is.null(pcode_col))     service_cols <- c(service_cols, pcode_col)

  all_cols <- names(data)
  potential_indicator_cols <- setdiff(all_cols, service_cols)

  indicator_cols <- potential_indicator_cols[
    sapply(data[potential_indicator_cols], function(x) is.numeric(x) || is.integer(x))
  ]

  if (length(indicator_cols) == 0) {
    stop("Не найдены числовые колонки-показатели в загружаемом файле")
  }

  # Извлекаем год и месяц
  if (!is.null(year_col)) {
    data$`.year` <- suppressWarnings(as.integer(data[[year_col]]))
  } else if (!is.null(date_col)) {
    dates <- tryCatch(as.Date(data[[date_col]]), error = function(e) rep(NA, nrow(data)))
    data$`.year` <- as.integer(format(dates, "%Y"))
    if (is.null(month_col)) {
      data$`.month` <- as.integer(format(dates, "%m"))
    }
  } else {
    data$`.year` <- rep(year_override %||% as.integer(format(Sys.Date(), "%Y")), nrow(data))
  }

  if (!is.null(month_col)) {
    data$`.month` <- suppressWarnings(as.integer(data[[month_col]]))
  } else if (!".month" %in% names(data)) {
    data$`.month` <- rep(month_override %||% NA_integer_, nrow(data))
  }

  data$`.entity_name` <- as.character(data[[entity_col]])

  # Сопоставление: сначала по ID/PCODE, потом по имени
  if (!is.null(entity_id_col) && !is.null(reference_data)) {
    # Прямое сопоставление по ID
    id_col_name <- if (entity_type == "district") "district_id" else "mo_id"
    data$`.entity_id` <- suppressWarnings(as.integer(data[[entity_id_col]]))
    valid_ids <- reference_data[[id_col_name]]
    data$`.entity_id`[!data$`.entity_id` %in% valid_ids] <- NA_integer_
  } else if (!is.null(pcode_col) && entity_type == "district" && !is.null(reference_data) && "district_pcode" %in% names(reference_data)) {
    # Сопоставление по PCODE
    data$`.entity_id` <- NA_integer_
    for (i in 1:nrow(data)) {
      pc <- as.character(data[[pcode_col]][i])
      if (is.na(pc)) next
      idx <- which(reference_data$district_pcode == pc)
      if (length(idx) > 0) data$`.entity_id`[i] <- reference_data$district_id[idx[1]]
    }
  } else if (!is.null(reference_data) && entity_type == "district") {
    data$`.entity_id` <- NA_integer_
    for (i in 1:nrow(data)) {
      name <- data$`.entity_name`[i]
      if (is.na(name) || name == "") next
      matches <- which(grepl(normalize_name(name),
                             normalize_name(reference_data$district_name_ru),
                             fixed = TRUE))
      if (length(matches) == 0) {
        matches <- which(sapply(
          normalize_name(reference_data$district_name_ru),
          function(x) grepl(x, normalize_name(name), fixed = TRUE)
        ))
      }
      if (length(matches) > 0) data$`.entity_id`[i] <- reference_data$district_id[matches[1]]
    }
  } else if (!is.null(reference_data) && entity_type == "mo") {
    mo_matches <- match_mo_names(data$`.entity_name`, reference_data)
    data$`.entity_id` <- mo_matches$matched_mo_id
  } else {
    data$`.entity_id` <- NA_integer_
  }

  # Pivot в длинный формат
  long_data <- data %>%
    dplyr::select(
      `.entity_id`, `.entity_name`, `.year`, `.month`,
      dplyr::all_of(indicator_cols)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(indicator_cols),
      names_to  = "indicator",
      values_to = "value"
    ) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::rename(
      entity_id   = `.entity_id`,
      entity_name = `.entity_name`,
      year        = `.year`,
      month       = `.month`
    ) %>%
    dplyr::mutate(import_date = Sys.time())

  return(long_data)
}

# === УТИЛИТА: NULL-COALESCENCE ===
`%||%` <- function(a, b) if (!is.null(a)) a else b
