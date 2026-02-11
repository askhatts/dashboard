# ============================================================
# УТИЛИТЫ ИМПОРТА ДАННЫХ ИЗ EXCEL
# ============================================================
# Функции для:
#   1) Нечёткого сопоставления названий МО (Jaro-Winkler)
#   2) Автодетекции колонок Excel
#   3) Конвертации широкого формата Excel в длинный формат
#   4) Валидации импортируемых данных
# ============================================================

# === НОРМАЛИЗАЦИЯ НАЗВАНИЙ ===
# Убирает все небуквенно-цифровые символы, приводит к нижнему регистру.
# Это позволяет сопоставлять названия независимо от пробелов,
# кавычек, скобок и регистра.
normalize_name <- function(name) {
  tolower(gsub("[^[:alnum:]]", "", as.character(name)))
}

# === СОПОСТАВЛЕНИЕ НАЗВАНИЙ МО ===
# Сравнивает список названий из Excel с названиями МО в базе.
# Использует три метода в порядке убывания точности:
#   1. Точное совпадение (после нормализации)
#   2. Нечёткое совпадение Jaro-Winkler (порог 70%)
#   3. Совпадение по номеру в названии (для "Поликлиника №7" → МО с №7)
#
# Параметры:
#   excel_names — вектор названий из Excel
#   db_mo       — data.frame МО из базы (нужны: mo_id, mo_name, mo_name_normalized)
#
# Возвращает: data.frame с колонками: excel_name, matched_mo_id, matched_mo_name,
#             confidence, match_method
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

    # --- Метод 2: Нечёткое совпадение (Jaro-Winkler) ---
    # Порог 70%: ниже — слишком много ложных срабатываний
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
    # Если в названии есть число (напр. "Поликлиника №7"),
    # ищем МО с таким же числом
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
        # Среди нескольких кандидатов с тем же номером — выбираем ближайшего по Jaro-Winkler
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
# Ищет в названиях колонок Excel ключевые слова
# для автоматического определения служебных колонок
# (ID/название сущности, дата, год, месяц).
# Все остальные числовые колонки считаются показателями.
#
# Параметры:
#   col_names   — вектор названий колонок Excel
#   entity_type — "district" или "mo" (определяет, что искать: район или МО)
auto_detect_columns <- function(col_names, entity_type = "district") {
  result <- list()
  cn <- tolower(col_names)

  # Ищем колонку-идентификатор сущности
  if (entity_type == "district") {
    # Для районов: ищем "район", "территория", "id_района"
    id_patterns <- "район|территор|district|id_район|регион"
  } else {
    # Для МО: ищем "мо", "организац", "учрежден", "клиник"
    id_patterns <- "мо|организац|учрежден|клиник|больниц|поликлиник|mo_name|id_мо"
  }

  id_idx <- which(grepl(id_patterns, cn))
  if (length(id_idx) > 0) result$entity_col <- col_names[id_idx[1]]

  # Ищем колонку года
  year_idx <- which(grepl("^год$|^year$|^data_year$", cn))
  if (length(year_idx) > 0) result$year_col <- col_names[year_idx[1]]

  # Ищем колонку месяца
  month_idx <- which(grepl("^месяц$|^month$|^data_month$", cn))
  if (length(month_idx) > 0) result$month_col <- col_names[month_idx[1]]

  # Ищем колонку даты (комбинированной)
  date_idx <- which(grepl("^дата$|^date$|дата_начала|start_date", cn))
  if (length(date_idx) > 0) result$date_col <- col_names[date_idx[1]]

  # Ищем координаты (для МО)
  lat_idx <- which(grepl("^широта$|^latitude$|^lat$|^x$", cn))
  if (length(lat_idx) > 0) result$lat_col <- col_names[lat_idx[1]]

  lon_idx <- which(grepl("^долгота$|^longitude$|^lon$|^lng$|^y$", cn))
  if (length(lon_idx) > 0) result$lon_col <- col_names[lon_idx[1]]

  return(result)
}

# === КОНВЕРТАЦИЯ EXCEL (ШИРОКИЙ ФОРМАТ) В ДЛИННЫЙ ФОРМАТ ===
# Ключевая функция для "динамического подтягивания столбцов".
# Принимает Excel-таблицу в широком формате (каждый показатель — отдельная колонка)
# и преобразует в длинный формат (indicator + value).
#
# Логика:
#   1. Определяем служебные колонки (ID, год, месяц) через auto_detect_columns()
#   2. Все ОСТАЛЬНЫЕ числовые колонки считаются показателями
#   3. Используем tidyr::pivot_longer() для конвертации
#
# Параметры:
#   data          — data.frame из readxl::read_excel()
#   entity_type   — "district" или "mo"
#   reference_data — data.frame справочника (районы или МО) для сопоставления названий
#   year_override — если год не найден в данных, использовать это значение
#   month_override — если месяц не найден, использовать это значение
parse_excel_to_long_format <- function(data, entity_type = "district",
                                        reference_data = NULL,
                                        year_override = NULL,
                                        month_override = NULL) {
  if (is.null(data) || nrow(data) == 0) {
    stop("Входные данные пусты")
  }

  # Шаг 1: Автодетекция колонок
  detected <- auto_detect_columns(names(data), entity_type)

  # Шаг 2: Определяем служебные колонки (не являются показателями)
  service_cols <- c()

  entity_col <- detected$entity_col
  if (is.null(entity_col)) {
    # Если не удалось найти — берём первую текстовую колонку
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

  if (!is.null(year_col))  service_cols <- c(service_cols, year_col)
  if (!is.null(month_col)) service_cols <- c(service_cols, month_col)
  if (!is.null(date_col))  service_cols <- c(service_cols, date_col)
  if (!is.null(lat_col))   service_cols <- c(service_cols, lat_col)
  if (!is.null(lon_col))   service_cols <- c(service_cols, lon_col)

  # Шаг 3: Все остальные числовые колонки = показатели
  all_cols <- names(data)
  potential_indicator_cols <- setdiff(all_cols, service_cols)

  # Оставляем только числовые колонки
  indicator_cols <- potential_indicator_cols[
    sapply(data[potential_indicator_cols], function(x) is.numeric(x) || is.integer(x))
  ]

  if (length(indicator_cols) == 0) {
    stop("Не найдены числовые колонки-показатели в загружаемом файле")
  }

  # Шаг 4: Извлекаем год и месяц
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

  # Шаг 5: Сопоставление с референсными данными (если предоставлены)
  if (!is.null(reference_data) && entity_type == "district") {
    # Нечёткое сопоставление по названию района
    data$`.entity_id` <- NA_integer_
    for (i in 1:nrow(data)) {
      name <- data$`.entity_name`[i]
      if (is.na(name) || name == "") next
      # Ищем район по частичному совпадению
      matches <- which(grepl(normalize_name(name),
                             normalize_name(reference_data$district_name_ru),
                             fixed = TRUE))
      if (length(matches) == 0) {
        # Обратный поиск
        matches <- which(sapply(
          normalize_name(reference_data$district_name_ru),
          function(x) grepl(x, normalize_name(name), fixed = TRUE)
        ))
      }
      if (length(matches) > 0) data$`.entity_id`[i] <- reference_data$district_id[matches[1]]
    }
  } else if (!is.null(reference_data) && entity_type == "mo") {
    # Нечёткое сопоставление по названию МО
    mo_matches <- match_mo_names(data$`.entity_name`, reference_data)
    data$`.entity_id` <- mo_matches$matched_mo_id
  } else {
    data$`.entity_id` <- NA_integer_
  }

  # Шаг 6: Pivot в длинный формат
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
# Аналог оператора %||% из rlang, если он не доступен.
# Возвращает левый аргумент, если он не NULL; иначе — правый.
`%||%` <- function(a, b) if (!is.null(a)) a else b
