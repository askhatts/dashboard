# ============================================================
# ПРИЛОЖЕНИЕ ИМПОРТА ДАННЫХ
# Версия: 2.1 (исправленная)
# ============================================================

library(shiny)
library(shinydashboard)
library(DBI)
library(RSQLite)
library(readxl)
library(dplyr)
library(DT)
library(stringdist)

# --- Вспомогательные функции ---

normalize_name <- function(name) {
  tolower(gsub("[^[:alnum:]]", "", as.character(name)))
}

match_mo_names <- function(excel_names, db_mo) {
  results <- data.frame(
    excel_name = excel_names,
    matched_mo_id = NA_integer_,
    matched_mo_name = NA_character_,
    confidence = NA_real_,
    match_method = NA_character_,
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(excel_names)) {
    if (is.na(excel_names[i]) || trimws(excel_names[i]) == "") next
    
    excel_norm <- normalize_name(excel_names[i])
    if (nchar(excel_norm) == 0) next
    
    # Метод 1: Точное совпадение
    exact <- which(db_mo$mo_name_normalized == excel_norm)
    if (length(exact) > 0) {
      results$matched_mo_id[i] <- db_mo$mo_id[exact[1]]
      results$matched_mo_name[i] <- db_mo$mo_name[exact[1]]
      results$confidence[i] <- 100
      results$match_method[i] <- "exact"
      next
    }
    
    # Метод 2: Jaro-Winkler
    distances <- stringdist::stringdist(excel_norm, db_mo$mo_name_normalized, method = "jw", p = 0.1)
    best_idx <- which.min(distances)
    similarity <- round((1 - distances[best_idx]) * 100)
    
    if (similarity >= 70) {
      results$matched_mo_id[i] <- db_mo$mo_id[best_idx]
      results$matched_mo_name[i] <- db_mo$mo_name[best_idx]
      results$confidence[i] <- similarity
      results$match_method[i] <- "fuzzy"
      next
    }
    
    # Метод 3: По номеру
    excel_num <- regmatches(excel_names[i], regexpr("\\d+", excel_names[i]))
    if (length(excel_num) > 0 && nchar(excel_num) > 0) {
      db_nums <- regmatches(db_mo$mo_name, regexpr("\\d+", db_mo$mo_name))
      num_matches <- which(db_nums == excel_num)
      if (length(num_matches) == 1) {
        results$matched_mo_id[i] <- db_mo$mo_id[num_matches]
        results$matched_mo_name[i] <- db_mo$mo_name[num_matches]
        results$confidence[i] <- 80
        results$match_method[i] <- "number"
      } else if (length(num_matches) > 1) {
        sub_dist <- stringdist::stringdist(excel_norm, db_mo$mo_name_normalized[num_matches], method="jw")
        sub_best <- which.min(sub_dist)
        results$matched_mo_id[i] <- db_mo$mo_id[num_matches[sub_best]]
        results$matched_mo_name[i] <- db_mo$mo_name[num_matches[sub_best]]
        results$confidence[i] <- round((1 - sub_dist[sub_best]) * 100)
        results$match_method[i] <- "number_fuzzy"
      }
    }
  }
  return(results)
}

# Автосопоставление колонок Excel с полями БД
auto_detect_columns <- function(col_names, screening_type = "breast") {
  result <- list()
  cn <- tolower(col_names)
  
  # МО
  mo_idx <- which(grepl("мо|организац|учрежден|mo_name|начавш", cn))
  if (length(mo_idx) > 0) result$mo_name <- col_names[mo_idx[1]]
  
  # Возраст
  age_idx <- which(grepl("возраст|age", cn))
  if (length(age_idx) > 0) result$age <- col_names[age_idx[1]]
  
  # Дата начала
  start_idx <- which(grepl("дата_начала|start_date|дата.*начал", cn))
  if (length(start_idx) > 0) result$start_date <- col_names[start_idx[1]]
  
  # Дата окончания
  end_idx <- which(grepl("дата_оконч|end_date|дата.*оконч", cn))
  if (length(end_idx) > 0) result$end_date <- col_names[end_idx[1]]
  
  # Отказ
  ref_idx <- which(grepl("отказ|refusal", cn))
  if (length(ref_idx) > 0) result$refusal <- col_names[ref_idx[1]]
  
  # Рак
  if (screening_type == "breast") {
    cancer_idx <- which(grepl("рак_молоч|breast_cancer|рак.*молоч", cn))
  } else {
    cancer_idx <- which(grepl("рак_толст|colorectal|рак.*толст|рак.*прям", cn))
  }
  if (length(cancer_idx) > 0) result$cancer <- col_names[cancer_idx[1]]
  
  # Биопсия
  bio_idx <- which(grepl("биопс|biopsy|трепано", cn))
  if (length(bio_idx) > 0) result$biopsy <- col_names[bio_idx[1]]
  
  # Координаты
  lat_idx <- which(grepl("^x$|широта|latitude|lat$", cn))
  if (length(lat_idx) > 0) result$latitude <- col_names[lat_idx[1]]
  
  lon_idx <- which(grepl("^y$|долгота|longitude|lon$|lng$", cn))
  if (length(lon_idx) > 0) result$longitude <- col_names[lon_idx[1]]
  
  return(result)
}

# ============ UI ============
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Импорт данных", titleWidth = 250),
  
  dashboardSidebar(
    width = 220,
    sidebarMenu(
      menuItem("Скрининг", tabName = "screening", icon = icon("stethoscope")),
      menuItem("Эпидемиология", tabName = "epidemiology", icon = icon("chart-bar")),
      menuItem("Координаты МО", tabName = "coords", icon = icon("map-pin"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # === СКРИНИНГ ===
      tabItem(tabName = "screening",
        box(title = "Шаг 1: Загрузка файла", status = "primary", solidHeader = TRUE, width = 12,
          fileInput("excel_file", "Выберите Excel:", accept = c(".xlsx", ".xls")),
          fluidRow(
            column(3, numericInput("scr_year", "Год:", value = as.integer(format(Sys.Date(),"%Y")), min = 2020, max = 2035)),
            column(3, numericInput("scr_month", "Месяц (1-12):", value = NA, min = 1, max = 12)),
            column(3, selectInput("scr_type", "Тип:", c("РМЖ"="breast","КРР"="colorectal"))),
            column(3, br(), actionButton("load_file", "Загрузить", icon=icon("upload"), class="btn-primary btn-block"))
          )
        ),
        box(title = "Шаг 2: Сопоставление колонок", status = "info", solidHeader = TRUE, width = 12,
            collapsible = TRUE, collapsed = TRUE,
          h5("Основные поля:"),
          fluidRow(
            column(3, selectInput("col_mo", "МО:", choices = NULL)),
            column(3, selectInput("col_age", "Возраст:", choices = NULL)),
            column(3, selectInput("col_start", "Дата начала:", choices = NULL)),
            column(3, selectInput("col_end", "Дата окончания:", choices = NULL))
          ),
          fluidRow(
            column(3, selectInput("col_refusal", "Отказ:", choices = NULL)),
            column(3, selectInput("col_expired", "Просрочено:", choices = NULL)),
            column(3, selectInput("col_cancer", "Рак:", choices = NULL)),
            column(3, selectInput("col_biopsy", "Биопсия:", choices = NULL))
          ),
          h5("Координаты (опционально):"),
          fluidRow(
            column(6, selectInput("col_lat", "Широта (X):", choices = NULL)),
            column(6, selectInput("col_lon", "Долгота (Y):", choices = NULL))
          ),
          hr(),
          actionButton("match_mo", "Сопоставить МО", icon=icon("link"), class="btn-info btn-lg")
        ),
        box(title = "Шаг 3: Проверка и импорт", status = "warning", solidHeader = TRUE, width = 12,
            collapsible = TRUE, collapsed = TRUE,
          DTOutput("matching_table"),
          hr(),
          fluidRow(
            column(6, actionButton("import_scr", "Импортировать данные", icon=icon("database"), class="btn-success btn-lg")),
            column(6, verbatimTextOutput("import_status"))
          )
        ),
        box(title = "Предпросмотр Excel", status = "success", solidHeader = TRUE, width = 12,
            collapsible = TRUE, collapsed = TRUE,
          DTOutput("preview_table"))
      ),
      
      # === ЭПИДЕМИОЛОГИЯ ===
      tabItem(tabName = "epidemiology",
        box(title = "Импорт эпидемиологии", status = "primary", solidHeader = TRUE, width = 12,
          p("Excel: Район | Заболеваемость | Смертность | ..."),
          fileInput("epi_file", "Excel:", accept = c(".xlsx", ".xls")),
          fluidRow(
            column(3, numericInput("epi_year", "Год:", value = as.integer(format(Sys.Date(),"%Y")), min=2020, max=2035)),
            column(3, numericInput("epi_month", "Месяц:", value = NA, min = 1, max = 12)),
            column(3, selectInput("epi_type", "Тип рака:", c("РМЖ"="breast","КРР"="colorectal","РШМ"="cervical","Все"="all"))),
            column(3, br(), actionButton("load_epi", "Загрузить", icon=icon("upload"), class="btn-primary"))
          ),
          h5("Колонки:"),
          fluidRow(
            column(3, selectInput("epi_col_district", "Район:", choices = NULL)),
            column(3, selectInput("epi_col_inc", "Заболеваемость:", choices = NULL)),
            column(3, selectInput("epi_col_mort", "Смертность:", choices = NULL)),
            column(3, selectInput("epi_col_pop", "Население:", choices = NULL))
          ),
          fluidRow(
            column(3, selectInput("epi_col_early", "Ранняя диагн.(%):", choices = NULL)),
            column(3, selectInput("epi_col_adv", "Запущенность:", choices = NULL)),
            column(3, selectInput("epi_col_1y", "1-лет. смерт.:", choices = NULL)),
            column(3, selectInput("epi_col_5y", "5-лет. выжив.:", choices = NULL))
          ),
          actionButton("import_epi", "Импортировать", icon=icon("database"), class="btn-success btn-lg")
        ),
        box(title = "Предпросмотр", status = "info", solidHeader = TRUE, width = 12,
            collapsible = TRUE, collapsed = TRUE,
          DTOutput("epi_preview"))
      ),
      
      # === КООРДИНАТЫ ===
      tabItem(tabName = "coords",
        box(title = "Обновление координат МО", status = "primary", solidHeader = TRUE, width = 12,
          p("Excel: Название МО | Широта | Долгота"),
          fileInput("coords_file", "Excel:", accept = c(".xlsx", ".xls")),
          fluidRow(
            column(4, selectInput("coords_col_name", "МО:", choices = NULL)),
            column(4, selectInput("coords_col_lat", "Широта:", choices = NULL)),
            column(4, selectInput("coords_col_lon", "Долгота:", choices = NULL))
          ),
          actionButton("load_coords_file", "Загрузить", icon=icon("upload"), class="btn-primary"),
          actionButton("import_coords", "Обновить координаты", icon=icon("map-pin"), class="btn-success btn-lg"),
          hr(),
          DTOutput("coords_preview")
        )
      )
    )
  )
)

# ============ SERVER ============
server <- function(input, output, session) {
  
  conn <- dbConnect(SQLite(), "data/abai_region.sqlite")
  
  excel_data <- reactiveVal(NULL)
  epi_data <- reactiveVal(NULL)
  coords_data <- reactiveVal(NULL)
  matched_data <- reactiveVal(NULL)
  
  db_mo <- dbGetQuery(conn, "SELECT mo_id, mo_name, mo_name_normalized FROM medical_organizations")
  db_districts <- dbGetQuery(conn, "SELECT district_id, district_name_ru FROM districts")
  
  # === СКРИНИНГ: Загрузка файла ===
  observeEvent(input$load_file, {
    req(input$excel_file)
    
    withProgress(message = "Чтение Excel...", {
      tryCatch({
        data <- read_excel(input$excel_file$datapath, guess_max = 5000)
        excel_data(data)
        
        col_choices <- c("-- Не выбрано --" = "", names(data))
        
        # Автоопределение колонок
        auto <- auto_detect_columns(names(data), input$scr_type)
        
        for (sel_id in c("col_mo","col_age","col_start","col_end","col_refusal","col_expired","col_cancer","col_biopsy","col_lat","col_lon")) {
          updateSelectInput(session, sel_id, choices = col_choices)
        }
        
        # Применяем автоопределение
        if (!is.null(auto$mo_name)) updateSelectInput(session, "col_mo", selected = auto$mo_name)
        if (!is.null(auto$age)) updateSelectInput(session, "col_age", selected = auto$age)
        if (!is.null(auto$start_date)) updateSelectInput(session, "col_start", selected = auto$start_date)
        if (!is.null(auto$end_date)) updateSelectInput(session, "col_end", selected = auto$end_date)
        if (!is.null(auto$refusal)) updateSelectInput(session, "col_refusal", selected = auto$refusal)
        if (!is.null(auto$cancer)) updateSelectInput(session, "col_cancer", selected = auto$cancer)
        if (!is.null(auto$biopsy)) updateSelectInput(session, "col_biopsy", selected = auto$biopsy)
        if (!is.null(auto$latitude)) updateSelectInput(session, "col_lat", selected = auto$latitude)
        if (!is.null(auto$longitude)) updateSelectInput(session, "col_lon", selected = auto$longitude)
        
        showNotification(sprintf("Загружено %d строк, %d колонок", nrow(data), ncol(data)), type="message")
      }, error = function(e) {
        showNotification(paste("Ошибка чтения:", e$message), type="error")
      })
    })
  })
  
  # === СКРИНИНГ: Сопоставление МО ===
  observeEvent(input$match_mo, {
    req(excel_data(), input$col_mo)
    if (input$col_mo == "") { showNotification("Выберите колонку МО!", type="error"); return() }
    
    withProgress(message = "Сопоставление МО...", {
      excel_mo <- unique(excel_data()[[input$col_mo]])
      matches <- match_mo_names(excel_mo, db_mo)
      matched_data(matches)
      
      found <- sum(!is.na(matches$matched_mo_id))
      total <- sum(!is.na(matches$excel_name) & matches$excel_name != "")
      showNotification(sprintf("Сопоставлено: %d из %d (%.0f%%)", found, total, found/max(total,1)*100), type="message")
    })
  })
  
  output$matching_table <- renderDT({
    req(matched_data())
    matched_data() %>%
      mutate(
        Статус = ifelse(is.na(matched_mo_id), "Не найдено", "Найдено"),
        Уверенность = ifelse(is.na(confidence), "-", paste0(confidence, "%"))
      ) %>%
      select(`В Excel` = excel_name, `В БД` = matched_mo_name, Метод = match_method, Статус, Уверенность) %>%
      datatable(options = list(pageLength=15), rownames = FALSE) %>%
      formatStyle("Статус", backgroundColor = styleEqual(c("Найдено","Не найдено"), c("#d4edda","#f8d7da")))
  })
  
  # === СКРИНИНГ: Импорт ===
  observeEvent(input$import_scr, {
    req(excel_data(), matched_data(), input$col_mo)
    
    withProgress(message = "Импорт...", value = 0, {
      data <- excel_data()
      matches <- matched_data()
      
      # Присоединяем mo_id
      import_df <- data %>%
        left_join(matches %>% select(excel_name, matched_mo_id),
                  by = setNames("excel_name", input$col_mo)) %>%
        filter(!is.na(matched_mo_id))
      
      if (nrow(import_df) == 0) {
        showNotification("Нет данных для импорта!", type="error")
        return()
      }
      
      table_name <- ifelse(input$scr_type == "breast", "screening_breast_detail", "screening_colorectal_detail")
      batch_id <- paste0("import_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      
      # Подготовка данных для batch insert
      safe_col <- function(col_id) {
        if (is.null(col_id) || col_id == "" || !col_id %in% names(import_df)) return(rep(NA, nrow(import_df)))
        import_df[[col_id]]
      }
      
      safe_date <- function(vals) {
        tryCatch(as.character(as.Date(vals)), error = function(e) rep(NA_character_, length(vals)))
      }
      
      safe_int <- function(vals) {
        suppressWarnings(as.integer(vals))
      }
      
      safe_bool <- function(vals) {
        v <- suppressWarnings(as.integer(vals))
        ifelse(!is.na(v) & v > 0, 1L, 0L)
      }
      
      # Даты
      start_dates <- safe_date(safe_col(input$col_start))
      start_years <- ifelse(!is.na(start_dates), as.integer(format(as.Date(start_dates), "%Y")), input$scr_year)
      start_months <- ifelse(!is.na(start_dates), as.integer(format(as.Date(start_dates), "%m")),
                             ifelse(!is.na(input$scr_month), input$scr_month, NA_integer_))
      
      # ИСПРАВЛЕНИЕ: Собираем ВСЕ доступные поля
      insert_data <- data.frame(
        mo_id = import_df$matched_mo_id,
        age = safe_int(safe_col(input$col_age)),
        start_date = start_dates,
        start_year = start_years,
        start_month = start_months,
        actual_end_date = safe_date(safe_col(input$col_end)),
        status_refusal = safe_bool(safe_col(input$col_refusal)),
        status_expired = safe_bool(safe_col(input$col_expired)),
        import_batch_id = batch_id,
        stringsAsFactors = FALSE
      )
      
      # Добавляем рак и биопсию
      if (input$scr_type == "breast") {
        insert_data$breast_cancer <- as.character(safe_col(input$col_cancer))
        insert_data$biopsy_performed <- safe_bool(safe_col(input$col_biopsy))
      } else {
        insert_data$colorectal_cancer <- as.character(safe_col(input$col_cancer))
        insert_data$biopsy_taken <- safe_bool(safe_col(input$col_biopsy))
      }
      
      # Обновление координат МО
      lats <- suppressWarnings(as.numeric(safe_col(input$col_lat)))
      lons <- suppressWarnings(as.numeric(safe_col(input$col_lon)))
      if (any(!is.na(lats) & !is.na(lons))) {
        coord_df <- data.frame(mo_id=import_df$matched_mo_id, lat=lats, lon=lons) %>%
          filter(!is.na(lat) & !is.na(lon)) %>%
          distinct(mo_id, .keep_all=TRUE)
        for (j in 1:nrow(coord_df)) {
          dbExecute(conn, "UPDATE medical_organizations SET latitude=?, longitude=?, custom_coords=1 WHERE mo_id=?",
                    params=list(coord_df$lat[j], coord_df$lon[j], coord_df$mo_id[j]))
        }
      }
      
      # ИСПРАВЛЕНИЕ: Batch insert в транзакции
      incProgress(0.3, detail="Запись в БД...")
      
      start_time <- Sys.time()
      dbBegin(conn)
      tryCatch({
        dbWriteTable(conn, table_name, insert_data, append = TRUE, row.names = FALSE)
        dbCommit(conn)
        
        duration <- as.numeric(difftime(Sys.time(), start_time, units="secs"))
        incProgress(0.5, detail="Пересчёт агрегатов...")
        
        # ИСПРАВЛЕНИЕ: Пересчёт агрегатов сразу после импорта
        source("R/app_functions.R", local = TRUE)
        calculate_aggregates(conn, input$scr_type, input$scr_year,
                            if(!is.na(input$scr_month)) input$scr_month else NULL)
        
        # Лог импорта
        tryCatch({
          dbExecute(conn, "INSERT INTO import_log (import_batch_id, import_type, file_name, records_total, records_imported, records_failed, duration_seconds, status)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
                    params=list(batch_id, paste0("screening_",input$scr_type),
                               basename(input$excel_file$name),
                               nrow(data), nrow(insert_data), nrow(data)-nrow(insert_data),
                               round(duration,1), "success"))
        }, error=function(e) {})
        
        showNotification(sprintf("Импортировано %d записей за %.1f сек", nrow(insert_data), duration),
                        type="message", duration=10)
        
      }, error = function(e) {
        dbRollback(conn)
        showNotification(paste("Ошибка импорта:", e$message), type="error", duration=NULL)
      })
    })
  })
  
  # === ЭПИДЕМИОЛОГИЯ ===
  observeEvent(input$load_epi, {
    req(input$epi_file)
    tryCatch({
      data <- read_excel(input$epi_file$datapath)
      epi_data(data)
      ch <- c("-- Не выбрано --"="", names(data))
      for (s in c("epi_col_district","epi_col_inc","epi_col_mort","epi_col_pop","epi_col_early","epi_col_adv","epi_col_1y","epi_col_5y")) {
        updateSelectInput(session, s, choices=ch)
      }
      showNotification(sprintf("Загружено %d строк", nrow(data)), type="message")
    }, error=function(e) showNotification(paste("Ошибка:", e$message), type="error"))
  })
  
  observeEvent(input$import_epi, {
    req(epi_data(), input$epi_col_district)
    if (input$epi_col_district == "") { showNotification("Выберите колонку Район!", type="error"); return() }
    
    withProgress(message = "Импорт эпидемиологии...", {
      data <- epi_data()
      
      # Удаляем старые данные за этот период
      dbExecute(conn, "DELETE FROM epidemiology_district WHERE data_year = ? AND cancer_type = ?",
                params=list(input$epi_year, input$epi_type))
      
      imported <- 0
      safe_num <- function(col_id, idx) {
        if (is.null(col_id) || col_id == "" || !col_id %in% names(data)) return(NA_real_)
        suppressWarnings(as.numeric(data[[col_id]][idx]))
      }
      
      dbBegin(conn)
      tryCatch({
        for (i in 1:nrow(data)) {
          district_name <- as.character(data[[input$epi_col_district]][i])
          if (is.na(district_name) || district_name == "") next
          
          # Поиск района (нечёткий)
          district_id <- dbGetQuery(conn, "SELECT district_id FROM districts WHERE district_name_ru LIKE ?",
                                    params=list(paste0("%", district_name, "%")))$district_id
          if (length(district_id) == 0) next
          
          inc <- safe_num(input$epi_col_inc, i)
          mort <- safe_num(input$epi_col_mort, i)
          pop <- safe_num(input$epi_col_pop, i)
          early <- safe_num(input$epi_col_early, i)
          adv <- safe_num(input$epi_col_adv, i)
          y1 <- safe_num(input$epi_col_1y, i)
          y5 <- safe_num(input$epi_col_5y, i)
          
          # Расчёт производных
          m2i <- if (!is.na(inc) && !is.na(mort) && inc > 0) mort / inc else NA_real_
          inc_rate <- if (!is.na(inc) && !is.na(pop) && pop > 0) (inc / pop) * 100000 else NA_real_
          mort_rate <- if (!is.na(mort) && !is.na(pop) && pop > 0) (mort / pop) * 100000 else NA_real_
          
          dbExecute(conn, "INSERT OR REPLACE INTO epidemiology_district 
            (district_id, data_year, data_month, cancer_type, incidence_count, incidence_rate,
             mortality_count, mortality_rate, mortality_to_incidence_ratio,
             early_stage_percent, advanced_stage_percent,
             one_year_mortality_percent, five_year_survival_percent, population)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
            params=list(district_id[1], input$epi_year,
                       if(is.na(input$epi_month)) NA_integer_ else input$epi_month,
                       input$epi_type, inc, inc_rate, mort, mort_rate, m2i,
                       early, adv, y1, y5, pop))
          imported <- imported + 1
        }
        dbCommit(conn)
        showNotification(sprintf("Импортировано %d записей эпидемиологии", imported), type="message", duration=5)
      }, error=function(e) {
        dbRollback(conn)
        showNotification(paste("Ошибка:", e$message), type="error")
      })
    })
  })
  
  # === КООРДИНАТЫ ===
  observeEvent(input$load_coords_file, {
    req(input$coords_file)
    tryCatch({
      data <- read_excel(input$coords_file$datapath)
      coords_data(data)
      ch <- c("-- Не выбрано --"="", names(data))
      updateSelectInput(session, "coords_col_name", choices=ch)
      updateSelectInput(session, "coords_col_lat", choices=ch)
      updateSelectInput(session, "coords_col_lon", choices=ch)
      showNotification("Файл координат загружен", type="message")
    }, error=function(e) showNotification(paste("Ошибка:", e$message), type="error"))
  })
  
  observeEvent(input$import_coords, {
    req(coords_data(), input$coords_col_name, input$coords_col_lat, input$coords_col_lon)
    if (input$coords_col_name == "") { showNotification("Выберите колонки!", type="error"); return() }
    
    data <- coords_data()
    updated <- 0
    
    for (i in 1:nrow(data)) {
      mo_name <- as.character(data[[input$coords_col_name]][i])
      lat <- suppressWarnings(as.numeric(data[[input$coords_col_lat]][i]))
      lon <- suppressWarnings(as.numeric(data[[input$coords_col_lon]][i]))
      
      if (is.na(mo_name) || is.na(lat) || is.na(lon)) next
      
      # Нечёткий поиск МО
      matches <- match_mo_names(mo_name, db_mo)
      if (!is.na(matches$matched_mo_id[1])) {
        dbExecute(conn, "UPDATE medical_organizations SET latitude=?, longitude=?, custom_coords=1 WHERE mo_id=?",
                  params=list(lat, lon, matches$matched_mo_id[1]))
        updated <- updated + 1
      }
    }
    showNotification(sprintf("Обновлено координат: %d из %d", updated, nrow(data)), type="message")
  })
  
  # --- Предпросмотры ---
  output$preview_table <- renderDT({
    req(excel_data())
    datatable(head(excel_data(), 100), options=list(scrollX=TRUE, pageLength=10))
  })
  output$epi_preview <- renderDT({
    req(epi_data())
    datatable(epi_data(), options=list(scrollX=TRUE, pageLength=10))
  })
  output$coords_preview <- renderDT({
    req(coords_data())
    datatable(coords_data(), options=list(scrollX=TRUE, pageLength=15))
  })
  output$import_status <- renderText({ "" })
  
  session$onSessionEnded(function() {
    tryCatch(dbDisconnect(conn), error=function(e){})
  })
}

shinyApp(ui, server)
