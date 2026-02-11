# ============================================================
# МОДУЛЬ: ПАНЕЛЬ УПРАВЛЕНИЯ (Селекторы индикаторов)
# ============================================================
# Содержит два выпадающих списка:
#   - Показатель А: Эпидемиология (для окраски районов)
#   - Показатель Б: Скрининги (для размера кругов МО)
# Списки заполняются ДИНАМИЧЕСКИ из загруженных данных.
# Также показывает информационные карточки (KPI).
# ============================================================

# === UI МОДУЛЯ ===
mod_controls_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Заголовок секции
    h4("Показатели"),

    # Информационные карточки (KPI)
    div(class = "info-box",
      div(class = "info-label", "Районов"),
      textOutput(ns("kpi_districts"), inline = TRUE) %>%
        tagAppendAttributes(class = "info-value")
    ),
    div(class = "info-box",
      div(class = "info-label", "Медорганизаций"),
      textOutput(ns("kpi_mo"), inline = TRUE) %>%
        tagAppendAttributes(class = "info-value")
    ),

    tags$hr(style = "border-color: #30305a;"),

    # --- Показатель А: Эпидемиология ---
    h4("Карта районов"),
    div(class = "info-label", style = "margin-bottom: 2px; color: #00d2ff;",
        "Показатель А (цвет)"),
    selectizeInput(
      ns("indicator_a"),
      label = NULL,
      choices = NULL,
      options = list(
        placeholder = "Выберите показатель...",
        allowEmptyOption = TRUE
      )
    ),

    tags$hr(style = "border-color: #30305a;"),

    # --- Показатель Б: Скрининги ---
    h4("Карта МО"),
    div(class = "info-label", style = "margin-bottom: 2px; color: #ff6b35;",
        "Показатель Б (размер)"),
    selectizeInput(
      ns("indicator_b"),
      label = NULL,
      choices = NULL,
      options = list(
        placeholder = "Выберите показатель...",
        allowEmptyOption = TRUE
      )
    ),

    tags$hr(style = "border-color: #30305a;"),

    # --- Информация о выбранных объектах ---
    h4("Выбрано на карте"),
    div(class = "info-box",
      div(class = "info-label", "Район"),
      textOutput(ns("selected_district_name"), inline = TRUE) %>%
        tagAppendAttributes(style = "color: #00d2ff; font-weight: 600; font-size: 13px;")
    ),
    div(class = "info-box",
      div(class = "info-label", "Медорганизация"),
      textOutput(ns("selected_mo_name"), inline = TRUE) %>%
        tagAppendAttributes(style = "color: #ff6b35; font-weight: 600; font-size: 13px;")
    )
  )
}

# === SERVER МОДУЛЯ ===
# Принимает общий reactiveValues (rv), обновляет списки показателей
# и записывает выбранные значения обратно в rv.
mod_controls_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Обновление списка показателей А (эпидемиология) ---
    # Срабатывает при изменении данных эпидемиологии (напр. после импорта)
    observe({
      epi <- rv$epi
      if (is.null(epi) || nrow(epi) == 0) {
        choices_a <- c("Нет данных" = "")
      } else {
        # Извлекаем уникальные названия показателей
        indicators <- sort(unique(epi$indicator))
        choices_a <- setNames(indicators, indicators)
      }

      updateSelectizeInput(session, "indicator_a",
                           choices = choices_a,
                           selected = if (length(choices_a) > 0) choices_a[1] else NULL)
    })

    # --- Обновление списка показателей Б (скрининг) ---
    observe({
      scr <- rv$scr
      if (is.null(scr) || nrow(scr) == 0) {
        choices_b <- c("Нет данных" = "")
      } else {
        indicators <- sort(unique(scr$indicator))
        choices_b <- setNames(indicators, indicators)
      }

      updateSelectizeInput(session, "indicator_b",
                           choices = choices_b,
                           selected = if (length(choices_b) > 0) choices_b[1] else NULL)
    })

    # --- Записываем выбранные индикаторы в общий rv ---
    # Чтобы другие модули (карта, графики) могли реагировать на изменения
    observe({
      rv$indicator_a <- input$indicator_a
    })

    observe({
      rv$indicator_b <- input$indicator_b
    })

    # --- KPI карточки ---
    output$kpi_districts <- renderText({
      if (!is.null(rv$districts)) nrow(rv$districts) else "0"
    })

    output$kpi_mo <- renderText({
      if (!is.null(rv$mo)) nrow(rv$mo) else "0"
    })

    # --- Названия выбранных объектов ---
    output$selected_district_name <- renderText({
      if (is.null(rv$selected_district_id)) return("Кликните на район")
      d <- rv$districts
      if (is.null(d)) return("-")
      row <- d[d$district_id == rv$selected_district_id, ]
      if (nrow(row) == 0) return("-")
      row$district_name_ru[1]
    })

    output$selected_mo_name <- renderText({
      if (is.null(rv$selected_mo_id)) return("Кликните на МО")
      m <- rv$mo
      if (is.null(m)) return("-")
      row <- m[m$mo_id == rv$selected_mo_id, ]
      if (nrow(row) == 0) return("-")
      row$mo_short_name[1]
    })
  })
}
