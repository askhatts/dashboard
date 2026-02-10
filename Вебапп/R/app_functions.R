# ============================================================
# ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ДЛЯ ПРИЛОЖЕНИЯ
# Версия: 2.1 (исправленная)
# ============================================================

library(leaflet)
library(dplyr)
library(plotly)

# Русские названия месяцев
MONTH_NAMES_RU <- c(
  "Январь", "Февраль", "Март", "Апрель", "Май", "Июнь",
  "Июль", "Август", "Сентябрь", "Октябрь", "Ноябрь", "Декабрь"
)

# Нормализация названий МО
normalize_name <- function(name) {
  tolower(gsub("[^[:alnum:]]", "", as.character(name)))
}

# ============================================================
# РАСЧЁТ АГРЕГАТОВ
# ============================================================

calculate_aggregates <- function(conn, screening_type = "breast", year = NULL, month = NULL) {
  
  detail_table <- ifelse(screening_type == "breast", 
                        "screening_breast_detail", 
                        "screening_colorectal_detail")
  
  agg_table <- ifelse(screening_type == "breast",
                     "screening_breast_aggregated",
                     "screening_colorectal_aggregated")
  
  # Фильтры
  filters <- "WHERE 1=1"
  params <- list()
  if (!is.null(year)) {
    filters <- paste0(filters, " AND start_year = ?")
    params <- c(params, list(as.integer(year)))
  }
  if (!is.null(month) && !is.na(month)) {
    filters <- paste0(filters, " AND start_month = ?")
    params <- c(params, list(as.integer(month)))
  }
  
  # ИСПРАВЛЕНИЕ: Полная агрегация всех полей
  if (screening_type == "breast") {
    query <- sprintf("
      SELECT 
        mo_id,
        start_year as data_year,
        start_month as data_month,
        COUNT(*) as total_started,
        SUM(CASE WHEN actual_end_date IS NOT NULL THEN 1 ELSE 0 END) as total_completed,
        SUM(CASE WHEN status_refusal = 1 THEN 1 ELSE 0 END) as total_refusals,
        SUM(CASE WHEN status_expired = 1 THEN 1 ELSE 0 END) as total_expired,
        SUM(CASE WHEN requires_intervention = 1 THEN 1 ELSE 0 END) as requires_intervention_count,
        SUM(CASE WHEN checklist_findings = 1 THEN 1 ELSE 0 END) as checklist_findings_count,
        SUM(CASE WHEN diagnosis_findings = 1 THEN 1 ELSE 0 END) as diagnosis_findings_count,
        SUM(CASE WHEN dispensary_by_detection = 1 THEN 1 ELSE 0 END) as dispensary_detection_count,
        SUM(CASE WHEN dispensary_other = 1 THEN 1 ELSE 0 END) as dispensary_other_count,
        SUM(CASE WHEN birads_m0_tech = 1 OR birads_m0_diag = 1 THEN 1 ELSE 0 END) as birads_m0_count,
        SUM(CASE WHEN birads_m1 = 1 THEN 1 ELSE 0 END) as birads_m1_count,
        SUM(CASE WHEN birads_m2 = 1 THEN 1 ELSE 0 END) as birads_m2_count,
        SUM(CASE WHEN birads_m3 = 1 THEN 1 ELSE 0 END) as birads_m3_count,
        SUM(CASE WHEN birads_m4_m5_mass = 1 OR birads_m4_m5_asymmetry = 1 
              OR birads_m4_m5_distortion = 1 OR birads_m4_m5_calcifications = 1 
            THEN 1 ELSE 0 END) as birads_m4_m5_count,
        SUM(CASE WHEN benign_neoplasm IS NOT NULL AND benign_neoplasm != '' THEN 1 ELSE 0 END) as benign_count,
        SUM(CASE WHEN breast_cancer IS NOT NULL AND breast_cancer != '' THEN 1 ELSE 0 END) as cancer_count,
        SUM(CASE WHEN biopsy_performed = 1 THEN 1 ELSE 0 END) as biopsy_count
      FROM %s
      %s
      GROUP BY mo_id, start_year, start_month
    ", detail_table, filters)
  } else {
    query <- sprintf("
      SELECT 
        mo_id,
        start_year as data_year,
        start_month as data_month,
        COUNT(*) as total_started,
        SUM(CASE WHEN actual_end_date IS NOT NULL THEN 1 ELSE 0 END) as total_completed,
        SUM(CASE WHEN status_refusal = 1 THEN 1 ELSE 0 END) as total_refusals,
        SUM(CASE WHEN status_expired = 1 THEN 1 ELSE 0 END) as total_expired,
        SUM(CASE WHEN hemoccult_total = 1 THEN 1 ELSE 0 END) as hemoccult_total,
        SUM(CASE WHEN hemoccult_positive = 1 THEN 1 ELSE 0 END) as hemoccult_positive,
        SUM(CASE WHEN hemoccult_negative = 1 THEN 1 ELSE 0 END) as hemoccult_negative,
        SUM(CASE WHEN hemoccult_doubtful = 1 THEN 1 ELSE 0 END) as hemoccult_doubtful,
        SUM(CASE WHEN colonoscopy_total = 1 THEN 1 ELSE 0 END) as colonoscopy_total,
        SUM(CASE WHEN cs4_polyps = 1 OR cs5_benign_tumors = 1 OR cs6_malignant_under_1cm_unverified = 1 
              OR cs7_malignant_under_1cm_verified = 1 OR cs8_malignant_over_1cm_unverified = 1 
              OR cs9_malignant_over_1cm_verified = 1
            THEN 1 ELSE 0 END) as colonoscopy_with_findings,
        SUM(CASE WHEN benign_neoplasm IS NOT NULL AND benign_neoplasm != '' THEN 1 ELSE 0 END) as benign_count,
        SUM(CASE WHEN colorectal_cancer IS NOT NULL AND colorectal_cancer != '' THEN 1 ELSE 0 END) as cancer_count,
        SUM(CASE WHEN biopsy_taken = 1 THEN 1 ELSE 0 END) as biopsy_count
      FROM %s
      %s
      GROUP BY mo_id, start_year, start_month
    ", detail_table, filters)
  }
  
  agg_data <- dbGetQuery(conn, query, params = if (length(params) > 0) params else NULL)
  
  if (nrow(agg_data) == 0) return(0)
  
  # ИСПРАВЛЕНИЕ: Расчёт производных показателей
  if (screening_type == "breast") {
    agg_data <- agg_data %>%
      mutate(
        detection_rate = ifelse(total_started > 0, (cancer_count / total_started) * 1000, NA_real_),
        ppv = ifelse((birads_m4_m5_count + cancer_count) > 0, 
                     (cancer_count / (birads_m4_m5_count + cancer_count)) * 100, NA_real_),
        completion_rate = ifelse(total_started > 0, (total_completed / total_started) * 100, NA_real_),
        refusal_rate = ifelse(total_started > 0, (total_refusals / total_started) * 100, NA_real_)
      )
  } else {
    agg_data <- agg_data %>%
      mutate(
        hemoccult_positive_percent = ifelse(hemoccult_total > 0, 
                                           (hemoccult_positive / hemoccult_total) * 100, NA_real_),
        detection_rate = ifelse(total_started > 0, (cancer_count / total_started) * 1000, NA_real_),
        completion_rate = ifelse(total_started > 0, (total_completed / total_started) * 100, NA_real_),
        colonoscopy_completion_rate = ifelse(hemoccult_positive > 0,
                                            (colonoscopy_total / hemoccult_positive) * 100, NA_real_)
      )
  }
  
  # Удаляем старые агрегаты для этих данных
  delete_filters <- "WHERE 1=1"
  delete_params <- list()
  if (!is.null(year)) {
    delete_filters <- paste0(delete_filters, " AND data_year = ?")
    delete_params <- c(delete_params, list(as.integer(year)))
  }
  if (!is.null(month) && !is.na(month)) {
    delete_filters <- paste0(delete_filters, " AND data_month = ?")
    delete_params <- c(delete_params, list(as.integer(month)))
  }
  
  dbExecute(conn, sprintf("DELETE FROM %s %s", agg_table, delete_filters),
            params = if (length(delete_params) > 0) delete_params else NULL)
  
  # Вставляем агрегаты через dbWriteTable (batch insert)
  # Подготовим данные для вставки, убрав столбцы которых нет в таблице
  agg_data$last_updated <- Sys.time()
  
  tryCatch({
    dbWriteTable(conn, agg_table, agg_data, append = TRUE, row.names = FALSE)
  }, error = function(e) {
    # Если batch не сработал, вставляем по одной
    warning("Batch insert не удался, переключаемся на построчную вставку: ", e$message)
    for (i in 1:nrow(agg_data)) {
      tryCatch({
        row <- as.list(agg_data[i, ])
        # Заменяем NaN на NA
        row <- lapply(row, function(x) if (is.nan(x)) NA else x)
        
        cols <- paste(names(row), collapse = ", ")
        placeholders <- paste(rep("?", length(row)), collapse = ", ")
        sql <- sprintf("INSERT OR REPLACE INTO %s (%s) VALUES (%s)", agg_table, cols, placeholders)
        dbExecute(conn, sql, params = unname(row))
      }, error = function(e2) {
        warning(sprintf("Ошибка вставки строки %d: %s", i, e2$message))
      })
    }
  })
  
  return(nrow(agg_data))
}

# ============================================================
# СОЗДАНИЕ КАРТЫ СКРИНИНГА
# ============================================================

create_screening_map <- function(conn, mo_data, screening_type, year, month, indicator) {
  
  table_name <- ifelse(screening_type == "breast", 
                      "screening_breast_aggregated", 
                      "screening_colorectal_aggregated")
  
  # ИСПРАВЛЕНИЕ: Приведение типов
  year <- as.integer(year)
  
  # Безопасная проверка indicator (защита от SQL-инъекций)
  allowed_breast <- c("total_started", "total_completed", "cancer_count", "biopsy_count",
                      "birads_m4_m5_count", "detection_rate", "ppv", "coverage_percent",
                      "completion_rate", "refusal_rate", "total_refusals", "benign_count",
                      "checklist_findings_count", "diagnosis_findings_count")
  allowed_colorectal <- c("total_started", "total_completed", "hemoccult_positive", 
                          "hemoccult_positive_percent", "colonoscopy_total", "cancer_count",
                          "detection_rate", "benign_count", "biopsy_count")
  
  allowed <- if (screening_type == "breast") allowed_breast else allowed_colorectal
  if (!indicator %in% allowed) {
    indicator <- "total_started"
  }
  
  # Построение запроса с параметрами
  params <- list(year)
  filters <- "WHERE data_year = ?"
  if (!is.na(month) && month != "all") {
    filters <- paste0(filters, " AND data_month = ?")
    params <- c(params, list(as.integer(month)))
  }
  
  query <- sprintf("
    SELECT mo_id, SUM(%s) as indicator_value
    FROM %s
    %s
    GROUP BY mo_id
  ", indicator, table_name, filters)
  
  agg_data <- dbGetQuery(conn, query, params = params)
  
  # Присоединяем к МО
  mo_with_data <- mo_data %>%
    left_join(agg_data, by = "mo_id") %>%
    mutate(indicator_value = ifelse(is.na(indicator_value), 0, indicator_value))
  
  # Фильтруем МО без координат
  mo_with_data <- mo_with_data %>%
    filter(!is.na(latitude) & !is.na(longitude))
  
  if (nrow(mo_with_data) == 0) {
    return(leaflet() %>% 
             addProviderTiles(providers$CartoDB.Positron) %>%
             setView(lng = 80.25, lat = 50.41, zoom = 7))
  }
  
  # Палитра
  max_val <- max(mo_with_data$indicator_value, na.rm = TRUE)
  if (max_val > 0) {
    pal <- colorNumeric("YlOrRd", domain = c(0, max_val))
  } else {
    pal <- colorNumeric("YlOrRd", domain = c(0, 1))
  }
  
  # Карта
  map <- leaflet(mo_with_data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lng = 80.25, lat = 50.41, zoom = 7) %>%
    addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      radius = ~pmin(pmax(sqrt(indicator_value) * 2, 5), 25),  # Адаптивный размер
      fillColor = ~pal(indicator_value),
      fillOpacity = 0.8,
      color = "white",
      weight = 2,
      popup = ~paste0(
        "<strong>", mo_short_name, "</strong><br>",
        "Район: ", district_name_ru, "<br>",
        "Тип: ", mo_type, "<br>",
        "<hr>",
        "<strong>Значение: ", round(indicator_value, 2), "</strong>"
      ),
      label = ~paste0(mo_short_name, ": ", round(indicator_value, 2))
    )
  
  if (max_val > 0) {
    map <- map %>%
      addLegend("bottomright", pal = pal, values = mo_with_data$indicator_value,
                title = indicator, opacity = 0.7)
  }
  
  return(map)
}

# ============================================================
# СОЗДАНИЕ КАРТЫ ЭПИДЕМИОЛОГИИ
# ============================================================

create_epi_map <- function(conn, districts_sf, cancer_type, year, indicator) {
  
  # Безопасная проверка indicator
  allowed_epi <- c("incidence_count", "incidence_rate", "mortality_count", "mortality_rate",
                   "mortality_to_incidence_ratio", "early_stage_percent", "advanced_stage_percent",
                   "one_year_mortality_percent", "five_year_survival_percent")
  if (!indicator %in% allowed_epi) indicator <- "incidence_count"
  
  year <- as.integer(year)
  
  query <- sprintf("
    SELECT district_id, %s as indicator_value
    FROM epidemiology_district
    WHERE data_year = ? AND cancer_type = ?
  ", indicator)
  
  epi_data <- dbGetQuery(conn, query, params = list(year, cancer_type))
  
  # Присоединяем к районам
  districts_with_data <- districts_sf %>%
    left_join(epi_data, by = "district_id") %>%
    mutate(indicator_value = ifelse(is.na(indicator_value), 0, indicator_value))
  
  # Палитра
  max_val <- max(districts_with_data$indicator_value, na.rm = TRUE)
  if (max_val > 0) {
    pal <- colorNumeric("YlOrRd", domain = c(0, max_val))
  } else {
    pal <- colorNumeric("YlOrRd", domain = c(0, 1))
  }
  
  map <- leaflet(districts_with_data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      fillColor = ~pal(indicator_value),
      weight = 2,
      opacity = 1,
      color = "white",
      fillOpacity = 0.7,
      popup = ~paste0(
        "<strong>", district_name_ru, "</strong><br>",
        "Тип: ", district_type, "<br>",
        "<hr>",
        "<strong>", indicator, ": ", round(indicator_value, 2), "</strong>"
      ),
      label = ~paste0(district_name_ru, ": ", round(indicator_value, 2)),
      highlightOptions = highlightOptions(
        weight = 4, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
      )
    )
  
  if (max_val > 0) {
    map <- map %>%
      addLegend("bottomright", pal = pal, values = districts_with_data$indicator_value,
                title = indicator, opacity = 0.7)
  }
  
  return(map)
}

# ============================================================
# ГРАФИКИ
# ============================================================

create_timeline_chart <- function(conn, screening_type, indicator, mo_ids = NULL) {
  
  table_name <- ifelse(screening_type == "breast", 
                      "screening_breast_aggregated", 
                      "screening_colorectal_aggregated")
  
  mo_filter <- ""
  params <- list()
  if (!is.null(mo_ids) && length(mo_ids) > 0) {
    placeholders <- paste(rep("?", length(mo_ids)), collapse = ",")
    mo_filter <- sprintf("WHERE mo_id IN (%s)", placeholders)
    params <- as.list(as.integer(mo_ids))
  }
  
  # Безопасный indicator
  query <- sprintf("
    SELECT 
      data_year,
      data_month,
      SUM(%s) as value
    FROM %s
    %s
    GROUP BY data_year, data_month
    ORDER BY data_year, data_month
  ", indicator, table_name, mo_filter)
  
  data <- dbGetQuery(conn, query, params = if (length(params) > 0) params else NULL)
  
  if (nrow(data) == 0) {
    return(plot_ly() %>% 
             layout(title = "Нет данных для отображения",
                    annotations = list(text = "Импортируйте данные скрининга",
                                      xref = "paper", yref = "paper",
                                      x = 0.5, y = 0.5, showarrow = FALSE)))
  }
  
  data <- data %>%
    mutate(
      period = ifelse(is.na(data_month), 
                     as.character(data_year),
                     paste0(data_year, "-", sprintf("%02d", data_month)))
    )
  
  plot_ly(data, x = ~period, y = ~value, type = "scatter", 
          mode = "lines+markers",
          line = list(color = "#3498db", width = 2),
          marker = list(color = "#3498db", size = 8)) %>%
    layout(
      xaxis = list(title = "Период", tickangle = -45),
      yaxis = list(title = "Значение"),
      hovermode = "x unified",
      margin = list(b = 80)
    )
}

create_mo_comparison_chart <- function(conn, screening_type, indicator, year, month = NULL) {
  
  table_name <- ifelse(screening_type == "breast", 
                      "screening_breast_aggregated", 
                      "screening_colorectal_aggregated")
  
  year <- as.integer(year)
  params <- list(year)
  filters <- "WHERE s.data_year = ?"
  
  if (!is.null(month) && !is.na(month) && month != "all") {
    filters <- paste0(filters, " AND s.data_month = ?")
    params <- c(params, list(as.integer(month)))
  }
  
  query <- sprintf("
    SELECT 
      m.mo_short_name,
      SUM(s.%s) as value
    FROM %s s
    JOIN medical_organizations m ON s.mo_id = m.mo_id
    %s
    GROUP BY m.mo_id, m.mo_short_name
    ORDER BY value DESC
    LIMIT 20
  ", indicator, table_name, filters)
  
  data <- dbGetQuery(conn, query, params = params)
  
  if (nrow(data) == 0) {
    return(plot_ly() %>% layout(title = "Нет данных"))
  }
  
  # Обрезаем длинные названия
  data$mo_short_name <- substr(data$mo_short_name, 1, 40)
  
  plot_ly(data, x = ~value, y = ~reorder(mo_short_name, value), 
          type = "bar", orientation = "h",
          marker = list(color = "#3498db")) %>%
    layout(
      xaxis = list(title = "Значение"),
      yaxis = list(title = "", tickfont = list(size = 10)),
      margin = list(l = 250)
    )
}

# Сравнение районов
create_district_comparison_chart <- function(conn, screening_type, indicator, year, month = NULL) {
  
  table_name <- ifelse(screening_type == "breast", 
                      "screening_breast_aggregated", 
                      "screening_colorectal_aggregated")
  
  year <- as.integer(year)
  params <- list(year)
  filters <- "WHERE s.data_year = ?"
  
  if (!is.null(month) && !is.na(month) && month != "all") {
    filters <- paste0(filters, " AND s.data_month = ?")
    params <- c(params, list(as.integer(month)))
  }
  
  query <- sprintf("
    SELECT 
      d.district_name_ru,
      SUM(s.%s) as value
    FROM %s s
    JOIN medical_organizations m ON s.mo_id = m.mo_id
    JOIN mo_district_link mdl ON m.mo_id = mdl.mo_id
    JOIN districts d ON mdl.district_id = d.district_id
    %s
    GROUP BY d.district_id, d.district_name_ru
    ORDER BY value DESC
  ", indicator, table_name, filters)
  
  data <- dbGetQuery(conn, query, params = params)
  
  if (nrow(data) == 0) {
    return(plot_ly() %>% layout(title = "Нет данных"))
  }
  
  plot_ly(data, x = ~reorder(district_name_ru, value), y = ~value, 
          type = "bar",
          marker = list(color = "#2ecc71")) %>%
    layout(
      xaxis = list(title = "", tickangle = -45),
      yaxis = list(title = "Значение"),
      margin = list(b = 120)
    )
}
