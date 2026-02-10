# ============================================================
# ГЛАВНОЕ ПРИЛОЖЕНИЕ - ДАШБОРД
# Версия: 2.1 (исправленная)
# ============================================================

library(shiny)
library(shinydashboard)
library(DBI)
library(RSQLite)
library(sf)
library(leaflet)
library(dplyr)
library(plotly)
library(DT)

source("R/app_functions.R")

# Данные при старте
init_conn <- dbConnect(SQLite(), "data/abai_region.sqlite")

districts_raw <- dbGetQuery(init_conn, "
  SELECT d.*, COUNT(DISTINCT mdl.mo_id) as mo_count
  FROM districts d
  LEFT JOIN mo_district_link mdl ON d.district_id = mdl.district_id
  GROUP BY d.district_id
")

districts_sf <- tryCatch({
  districts_raw %>%
    mutate(geometry = st_as_sfc(geometry_wkt, crs = 4326)) %>%
    st_as_sf()
}, error = function(e) { warning("SF error: ", e$message); NULL })

mo_data <- dbGetQuery(init_conn, "
  SELECT m.*, d.district_name_ru, d.district_name_en
  FROM medical_organizations m
  JOIN mo_district_link mdl ON m.mo_id = mdl.mo_id
  JOIN districts d ON mdl.district_id = d.district_id
  WHERE m.active = 1
")

dbDisconnect(init_conn)

# ============ UI ============
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Скрининг Абайской области", titleWidth = 300),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(id = "tabs",
      menuItem("Обзор", tabName = "overview", icon = icon("dashboard")),
      menuItem("Карта скрининга", tabName = "map_screening", icon = icon("map")),
      menuItem("Карта эпидемиологии", tabName = "map_epi", icon = icon("globe")),
      menuItem("Графики", tabName = "charts", icon = icon("chart-line")),
      menuItem("Таблицы", tabName = "tables", icon = icon("table")),
      menuItem("МО", tabName = "mo", icon = icon("hospital"))
    ),
    hr(),
    conditionalPanel(
      condition = "input.tabs == 'map_screening' || input.tabs == 'charts'",
      h5("Фильтры скрининга", style = "padding-left:15px;font-weight:bold;"),
      selectInput("screening_type", "Тип:", c("РМЖ"="breast","КРР"="colorectal")),
      uiOutput("filter_year_ui"),
      uiOutput("filter_month_ui"),
      uiOutput("filter_indicator_ui")
    ),
    conditionalPanel(
      condition = "input.tabs == 'map_epi'",
      h5("Фильтры эпидемиологии", style = "padding-left:15px;font-weight:bold;"),
      uiOutput("filter_epi_year_ui"),
      selectInput("epi_cancer_type", "Тип рака:",
                  c("РМЖ"="breast","КРР"="colorectal","РШМ"="cervical","Все"="all")),
      uiOutput("filter_epi_indicator_ui")
    ),
    hr(),
    actionButton("btn_refresh", "Обновить агрегаты", icon=icon("sync"),
                class="btn-primary btn-block", style="margin:10px;")
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML(".content-wrapper{background-color:#f4f6f9;}"))),
    tabItems(
      # ОБЗОР
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("box_districts", width=3),
          valueBoxOutput("box_mo", width=3),
          valueBoxOutput("box_screening", width=3),
          valueBoxOutput("box_epi", width=3)
        ),
        fluidRow(
          box(title="МО по районам", status="primary", solidHeader=TRUE,
              plotlyOutput("plot_mo_by_district", height="350px"), width=6),
          box(title="Типы МО", status="success", solidHeader=TRUE,
              plotlyOutput("plot_mo_types", height="350px"), width=6)
        ),
        fluidRow(
          box(title="Статус данных", status="info", solidHeader=TRUE, width=12,
              collapsible=TRUE, htmlOutput("data_status"))
        )
      ),
      # КАРТА СКРИНИНГА
      tabItem(tabName = "map_screening",
        box(title=textOutput("map_scr_title"), status="primary", solidHeader=TRUE, width=12,
            leafletOutput("map_screening_out", height="700px"))
      ),
      # КАРТА ЭПИДЕМИОЛОГИИ
      tabItem(tabName = "map_epi",
        box(title="Карта эпидемиологии", status="warning", solidHeader=TRUE, width=12,
            leafletOutput("map_epi_out", height="700px"))
      ),
      # ГРАФИКИ
      tabItem(tabName = "charts",
        box(title="Динамика", status="primary", solidHeader=TRUE, width=12,
            plotlyOutput("chart_timeline", height="400px")),
        fluidRow(
          box(title="Сравнение МО", status="info", solidHeader=TRUE, width=6,
              plotlyOutput("chart_compare_mo", height="500px")),
          box(title="Сравнение районов", status="success", solidHeader=TRUE, width=6,
              plotlyOutput("chart_compare_districts", height="500px"))
        )
      ),
      # ТАБЛИЦЫ
      tabItem(tabName = "tables",
        box(title="Данные скрининга", status="primary", solidHeader=TRUE, width=12,
            selectInput("table_scr_type","Тип:",c("РМЖ"="breast","КРР"="colorectal"),width="200px"),
            DTOutput("table_screening"),
            br(), downloadButton("dl_screening","Скачать CSV",class="btn-success")),
        box(title="Эпидемиология", status="warning", solidHeader=TRUE, width=12,
            DTOutput("table_epi"),
            br(), downloadButton("dl_epi","Скачать CSV",class="btn-success"))
      ),
      # МО
      tabItem(tabName = "mo",
        box(title="Справочник МО", status="info", solidHeader=TRUE, width=12,
            DTOutput("table_mo"))
      )
    )
  )
)

# ============ SERVER ============
server <- function(input, output, session) {
  
  # ИСПРАВЛЕНИЕ: Единственное соединение (не reactive)
  db_conn <- dbConnect(SQLite(), "data/abai_region.sqlite")
  session$onSessionEnded(function() {
    tryCatch(dbDisconnect(db_conn), error=function(e){})
  })
  
  # --- Фильтры ---
  output$filter_year_ui <- renderUI({
    tbl <- ifelse(input$screening_type=="breast","screening_breast_detail","screening_colorectal_detail")
    years <- tryCatch(
      dbGetQuery(db_conn, sprintf("SELECT DISTINCT start_year FROM %s WHERE start_year IS NOT NULL ORDER BY start_year DESC", tbl))$start_year,
      error=function(e) integer(0))
    if (length(years)==0) years <- as.integer(format(Sys.Date(),"%Y"))
    selectInput("filter_year","Год:",choices=years,selected=years[1])
  })
  
  output$filter_month_ui <- renderUI({
    # ИСПРАВЛЕНИЕ: русские месяцы
    selectInput("filter_month","Месяц:",
      choices=c("Все"="all", setNames(1:12, MONTH_NAMES_RU)), selected="all")
  })
  
  output$filter_indicator_ui <- renderUI({
    if (is.null(input$screening_type)) return(NULL)
    ch <- if (input$screening_type=="breast") {
      c("Всего начато"="total_started","Завершено"="total_completed",
        "Рак"="cancer_count","Биопсий"="biopsy_count",
        "BI-RADS M4-M5"="birads_m4_m5_count",
        "Выявляемость (1000)"="detection_rate","PPV (%)"="ppv")
    } else {
      c("Всего начато"="total_started","Гемокульт+"="hemoccult_positive",
        "Колоноскопий"="colonoscopy_total","Рак"="cancer_count",
        "Выявляемость (1000)"="detection_rate")
    }
    selectInput("filter_indicator","Показатель:",choices=ch)
  })
  
  output$filter_epi_year_ui <- renderUI({
    years <- tryCatch(
      dbGetQuery(db_conn,"SELECT DISTINCT data_year FROM epidemiology_district ORDER BY data_year DESC")$data_year,
      error=function(e) integer(0))
    if (length(years)==0) years <- as.integer(format(Sys.Date(),"%Y"))
    selectInput("filter_epi_year","Год:",choices=years,selected=years[1])
  })
  
  output$filter_epi_indicator_ui <- renderUI({
    selectInput("filter_epi_indicator","Показатель:",choices=c(
      "Заболеваемость"="incidence_count","Смертность"="mortality_count",
      "Смерт./Забол."="mortality_to_incidence_ratio",
      "Ранняя диагн. (%)"="early_stage_percent",
      "Запущенность (%)"="advanced_stage_percent",
      "5-лет. выжив. (%)"="five_year_survival_percent"))
  })
  
  # --- Value boxes ---
  output$box_districts <- renderValueBox(valueBox(nrow(districts_raw),"Районов",icon=icon("map"),color="blue"))
  output$box_mo <- renderValueBox(valueBox(nrow(mo_data),"МО",icon=icon("hospital"),color="green"))
  output$box_screening <- renderValueBox({
    cnt <- tryCatch(
      dbGetQuery(db_conn,"SELECT (SELECT COUNT(*) FROM screening_breast_detail)+(SELECT COUNT(*) FROM screening_colorectal_detail) as cnt")$cnt,
      error=function(e) 0)
    valueBox(cnt,"Записей скрининга",icon=icon("stethoscope"),color="yellow")
  })
  output$box_epi <- renderValueBox({
    cnt <- tryCatch(dbGetQuery(db_conn,"SELECT COUNT(*) as cnt FROM epidemiology_district")$cnt, error=function(e) 0)
    valueBox(cnt,"Эпидемиология",icon=icon("chart-bar"),color="red")
  })
  
  # --- Обзор ---
  output$plot_mo_by_district <- renderPlotly({
    pd <- mo_data %>% count(district_name_ru) %>% arrange(desc(n))
    plot_ly(pd, x=~reorder(district_name_ru,n), y=~n, type="bar",
            marker=list(color="#3498db"), text=~n, textposition="outside") %>%
      layout(xaxis=list(title="",tickangle=-45), yaxis=list(title="Кол-во"), margin=list(b=120))
  })
  output$plot_mo_types <- renderPlotly({
    pd <- mo_data %>% count(mo_type)
    plot_ly(pd, labels=~mo_type, values=~n, type="pie",
            marker=list(colors=c("#3498db","#2ecc71","#f39c12","#e74c3c","#95a5a6")))
  })
  output$data_status <- renderUI({
    b <- tryCatch(dbGetQuery(db_conn,"SELECT COUNT(*) FROM screening_breast_detail")[[1]],error=function(e)0)
    c <- tryCatch(dbGetQuery(db_conn,"SELECT COUNT(*) FROM screening_colorectal_detail")[[1]],error=function(e)0)
    e <- tryCatch(dbGetQuery(db_conn,"SELECT COUNT(*) FROM epidemiology_district")[[1]],error=function(e)0)
    ba <- tryCatch(dbGetQuery(db_conn,"SELECT COUNT(*) FROM screening_breast_aggregated")[[1]],error=function(e)0)
    mk <- function(v) if(v>0) "<span style='color:green;'>&#10004;</span>" else "<span style='color:gray;'>-</span>"
    HTML(paste0("<table class='table table-striped' style='max-width:500px;'>",
      "<tr><th>Данные</th><th>Записей</th><th></th></tr>",
      sprintf("<tr><td>РМЖ детально</td><td>%d</td><td>%s</td></tr>",b,mk(b)),
      sprintf("<tr><td>КРР детально</td><td>%d</td><td>%s</td></tr>",c,mk(c)),
      sprintf("<tr><td>Агрегаты РМЖ</td><td>%d</td><td>%s</td></tr>",ba,mk(ba)),
      sprintf("<tr><td>Эпидемиология</td><td>%d</td><td>%s</td></tr>",e,mk(e)),
      "</table><p><em>Импорт: launcher.R &rarr; опция 2</em></p>"))
  })
  
  # --- Карты ---
  output$map_scr_title <- renderText({
    req(input$filter_indicator, input$screening_type)
    tp <- ifelse(input$screening_type=="breast","РМЖ","КРР")
    paste("Карта скрининга", tp)
  })
  
  output$map_screening_out <- renderLeaflet({
    req(input$filter_year, input$filter_indicator)
    tryCatch(
      create_screening_map(db_conn, mo_data, input$screening_type,
                          input$filter_year, input$filter_month, input$filter_indicator),
      error=function(e) leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(80.25,50.41,7))
  })
  
  output$map_epi_out <- renderLeaflet({
    req(input$filter_epi_year, input$filter_epi_indicator)
    if (is.null(districts_sf)) return(leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(80.25,50.41,7))
    tryCatch(
      create_epi_map(db_conn, districts_sf, input$epi_cancer_type,
                     input$filter_epi_year, input$filter_epi_indicator),
      error=function(e) leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(80.25,50.41,7))
  })
  
  # --- Графики ---
  output$chart_timeline <- renderPlotly({
    req(input$filter_indicator)
    tryCatch(create_timeline_chart(db_conn, input$screening_type, input$filter_indicator),
             error=function(e) plot_ly() %>% layout(title="Нет данных"))
  })
  output$chart_compare_mo <- renderPlotly({
    req(input$filter_year, input$filter_indicator)
    tryCatch(create_mo_comparison_chart(db_conn, input$screening_type, input$filter_indicator,
                                        input$filter_year, input$filter_month),
             error=function(e) plot_ly() %>% layout(title="Нет данных"))
  })
  output$chart_compare_districts <- renderPlotly({
    req(input$filter_year, input$filter_indicator)
    tryCatch(create_district_comparison_chart(db_conn, input$screening_type, input$filter_indicator,
                                              input$filter_year, input$filter_month),
             error=function(e) plot_ly() %>% layout(title="Нет данных"))
  })
  
  # --- Таблицы ---
  scr_data <- reactive({
    type <- if(is.null(input$table_scr_type)) "breast" else input$table_scr_type
    tbl <- ifelse(type=="breast","screening_breast_aggregated","screening_colorectal_aggregated")
    if (type=="breast") {
      q <- sprintf("SELECT m.mo_short_name as 'МО', d.district_name_ru as 'Район',
        s.data_year as 'Год', s.data_month as 'Месяц',
        s.total_started as 'Начато', s.total_completed as 'Завершено',
        s.cancer_count as 'Рак', s.biopsy_count as 'Биопсий',
        ROUND(s.detection_rate,2) as 'Выявляемость', ROUND(s.ppv,1) as 'PPV'
        FROM %s s JOIN medical_organizations m ON s.mo_id=m.mo_id
        JOIN mo_district_link mdl ON m.mo_id=mdl.mo_id
        JOIN districts d ON mdl.district_id=d.district_id
        ORDER BY s.data_year DESC, s.total_started DESC", tbl)
    } else {
      q <- sprintf("SELECT m.mo_short_name as 'МО', d.district_name_ru as 'Район',
        s.data_year as 'Год', s.data_month as 'Месяц',
        s.total_started as 'Начато', s.hemoccult_positive as 'Гемокульт+',
        s.colonoscopy_total as 'Колоноскопий', s.cancer_count as 'Рак',
        ROUND(s.detection_rate,2) as 'Выявляемость'
        FROM %s s JOIN medical_organizations m ON s.mo_id=m.mo_id
        JOIN mo_district_link mdl ON m.mo_id=mdl.mo_id
        JOIN districts d ON mdl.district_id=d.district_id
        ORDER BY s.data_year DESC, s.total_started DESC", tbl)
    }
    tryCatch(dbGetQuery(db_conn, q), error=function(e) data.frame())
  })
  
  output$table_screening <- renderDT(datatable(scr_data(), options=list(pageLength=25,scrollX=TRUE), filter="top", rownames=FALSE))
  output$dl_screening <- downloadHandler(
    filename=function() paste0("screening_",Sys.Date(),".csv"),
    content=function(file) write.csv(scr_data(), file, row.names=FALSE, fileEncoding="UTF-8"))
  
  epi_data_r <- reactive({
    tryCatch(dbGetQuery(db_conn, "
      SELECT d.district_name_ru as 'Район', e.data_year as 'Год',
        CASE e.cancer_type WHEN 'breast' THEN 'РМЖ' WHEN 'colorectal' THEN 'КРР' WHEN 'cervical' THEN 'РШМ' ELSE 'Все' END as 'Тип',
        e.incidence_count as 'Заболеваемость', e.mortality_count as 'Смертность',
        ROUND(e.early_stage_percent,1) as 'Ранняя диагн.(%)',
        ROUND(e.advanced_stage_percent,1) as 'Запущенность(%)',
        ROUND(e.five_year_survival_percent,1) as '5-лет.выжив.(%)'
      FROM epidemiology_district e JOIN districts d ON e.district_id=d.district_id
      ORDER BY e.data_year DESC"), error=function(e) data.frame())
  })
  
  output$table_epi <- renderDT(datatable(epi_data_r(), options=list(pageLength=15,scrollX=TRUE), filter="top", rownames=FALSE))
  output$dl_epi <- downloadHandler(
    filename=function() paste0("epi_",Sys.Date(),".csv"),
    content=function(file) write.csv(epi_data_r(), file, row.names=FALSE, fileEncoding="UTF-8"))
  
  output$table_mo <- renderDT({
    mo_data %>%
      select(МО=mo_short_name, Район=district_name_ru, Тип=mo_type,
             Собственность=ownership, Широта=latitude, Долгота=longitude) %>%
      datatable(options=list(pageLength=30,scrollX=TRUE), filter="top", rownames=FALSE)
  })
  
  # --- Обновление ---
  observeEvent(input$btn_refresh, {
    showNotification("Пересчёт агрегатов...", type="message", duration=2)
    tryCatch({
      nb <- calculate_aggregates(db_conn, "breast")
      nc <- calculate_aggregates(db_conn, "colorectal")
      showNotification(sprintf("Готово: %d РМЖ, %d КРР агрегатов", nb, nc), type="message", duration=5)
    }, error=function(e) showNotification(paste("Ошибка:",e$message), type="error"))
  })
}

shinyApp(ui, server)
