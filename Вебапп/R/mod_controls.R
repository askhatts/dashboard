# ============================================================
# МОДУЛЬ: ПАНЕЛЬ УПРАВЛЕНИЯ (Селекторы индикаторов + KPI)
# ============================================================
# Содержит:
#   - Динамические KPI-карточки (средние эпид. показатели региона)
#   - Показатель А: Эпидемиология (для окраски районов)
#   - Показатель Б: Скрининги (для размера кругов МО)
#   - Информация о выбранных объектах
# ============================================================

# === UI МОДУЛЯ ===
mod_controls_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Заголовок секции
    h4("Показатели"),

    # Динамические KPI-карточки (средние по региону)
    div(class = "info-box",
      div(class = "info-label", "Заболеваемость"),
      textOutput(ns("kpi_incidence"), inline = TRUE) %>%
        tagAppendAttributes(class = "info-value")
    ),
    div(class = "info-box",
      div(class = "info-label", "Смертность"),
      textOutput(ns("kpi_mortality"), inline = TRUE) %>%
        tagAppendAttributes(class = "info-value")
    ),
    div(class = "info-box",
      div(class = "info-label", "Ранняя диагн. %"),
      textOutput(ns("kpi_early_diag"), inline = TRUE) %>%
        tagAppendAttributes(class = "info-value")
    ),
    div(class = "info-box",
      div(class = "info-label", "Запущенность %"),
      textOutput(ns("kpi_advanced"), inline = TRUE) %>%
        tagAppendAttributes(class = "info-value")
    ),
    div(class = "info-box",
      div(class = "info-label", "5-лет. выжив. %"),
      textOutput(ns("kpi_survival"), inline = TRUE) %>%
        tagAppendAttributes(class = "info-value")
    ),
    div(class = "info-box",
      div(class = "info-label", "Смерт./Забол. %"),
      textOutput(ns("kpi_mort_inc_ratio"), inline = TRUE) %>%
        tagAppendAttributes(class = "info-value", style = "color: #ffc107;")
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
mod_controls_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # === УТИЛИТА: Средний показатель по региону ===
    # Ищет показатель по паттерну в названии, считает среднее по районам
    avg_epi_indicator <- function(pattern) {
      epi <- rv$epi
      if (is.null(epi) || nrow(epi) == 0) return(NA_real_)

      indicators <- unique(epi$indicator)
      matched <- indicators[grepl(pattern, indicators, ignore.case = TRUE)]
      if (length(matched) == 0) return(NA_real_)

      # Берём последний период (максимальный год/месяц)
      target <- epi %>%
        filter(indicator == matched[1])

      if (nrow(target) == 0) return(NA_real_)

      # Среднее по районам (последний доступный период)
      latest <- target %>%
        filter(year == max(year, na.rm = TRUE))
      if (any(!is.na(latest$month))) {
        latest <- latest %>% filter(month == max(month, na.rm = TRUE))
      }

      mean(latest$value, na.rm = TRUE)
    }

    # --- Обновление списка показателей А (эпидемиология) ---
    observe({
      epi <- rv$epi
      if (is.null(epi) || nrow(epi) == 0) {
        choices_a <- c("Нет данных" = "")
      } else {
        indicators <- sort(unique(epi$indicator))
        # Добавляем вычисляемый показатель "Смертность/Заболеваемость"
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
    observe({
      rv$indicator_a <- input$indicator_a
    })

    observe({
      rv$indicator_b <- input$indicator_b
    })

    # --- Динамические KPI карточки (средние по региону) ---
    format_kpi <- function(val) {
      if (is.na(val) || is.null(val)) return("\u2014")
      round(val, 1)
    }

    output$kpi_incidence <- renderText({
      format_kpi(avg_epi_indicator("заболеваемость"))
    })

    output$kpi_mortality <- renderText({
      format_kpi(avg_epi_indicator("смертность"))
    })

    output$kpi_early_diag <- renderText({
      format_kpi(avg_epi_indicator("ранняя диагностика|ранняя_диагностика"))
    })

    output$kpi_advanced <- renderText({
      format_kpi(avg_epi_indicator("запущенность"))
    })

    output$kpi_survival <- renderText({
      format_kpi(avg_epi_indicator("выживаемость"))
    })

    output$kpi_mort_inc_ratio <- renderText({
      mort <- avg_epi_indicator("смертность")
      inc  <- avg_epi_indicator("заболеваемость")
      if (is.na(mort) || is.na(inc) || inc == 0) return("\u2014")
      format_kpi(mort / inc * 100)
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
