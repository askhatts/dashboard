# ============================================================
# МОДУЛЬ: ГРАФИКИ ДИНАМИКИ (Зоны А и Б)
# ============================================================
# Универсальный модуль, который создаётся дважды:
#   - Зона А (type = "epi"): линейный график эпидемиологии
#     для района, выбранного кликом на карте
#   - Зона Б (type = "scr"): линейный график скрининга
#     для МО, выбранной кликом на карте
#
# Клик по точке на графике → обновляет rv$selected_period_a/b,
# что фильтрует карту по выбранному периоду.
# ============================================================

# === UI МОДУЛЯ ===
mod_charts_ui <- function(id) {
  ns <- NS(id)

  # Plotly-график, растянутый на всю доступную высоту панели
  div(class = "panel-body",
    plotlyOutput(ns("chart"), height = "100%")
  )
}

# === SERVER МОДУЛЯ ===
# Параметры:
#   id   — идентификатор экземпляра модуля
#   rv   — общий reactiveValues (shared state)
#   type — "epi" (эпидемиология) или "scr" (скрининг)
mod_charts_server <- function(id, rv, type = "epi") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Уникальный source ID для plotly events (избегает конфликта между экземплярами)
    plotly_source <- ns("plotly_src")

    # === РЕАКТИВНЫЕ ДАННЫЕ ДЛЯ ГРАФИКА ===
    chart_data <- reactive({
      if (type == "epi") {
        district_id <- rv$selected_district_id
        indicator   <- rv$indicator_a
        epi         <- rv$epi

        if (is.null(district_id) || is.null(indicator) || indicator == "") {
          return(NULL)
        }
        if (is.null(epi) || nrow(epi) == 0) return(NULL)

        filtered <- epi %>%
          filter(
            district_id == !!district_id,
            indicator == !!indicator
          ) %>%
          arrange(year, month)

        return(filtered)

      } else {
        mo_id     <- rv$selected_mo_id
        indicator <- rv$indicator_b
        scr       <- rv$scr

        if (is.null(mo_id) || is.null(indicator) || indicator == "") {
          return(NULL)
        }
        if (is.null(scr) || nrow(scr) == 0) return(NULL)

        filtered <- scr %>%
          filter(
            mo_id == !!mo_id,
            indicator == !!indicator
          ) %>%
          arrange(year, month)

        return(filtered)
      }
    })

    # === ЗАГОЛОВОК ГРАФИКА ===
    chart_title <- reactive({
      if (type == "epi") {
        d_id <- rv$selected_district_id
        if (is.null(d_id)) return(NULL)
        d <- rv$districts
        if (is.null(d)) return(NULL)
        row <- d[d$district_id == d_id, ]
        if (nrow(row) == 0) return(NULL)
        paste0(row$district_name_ru[1], " \u2014 ", rv$indicator_a)
      } else {
        m_id <- rv$selected_mo_id
        if (is.null(m_id)) return(NULL)
        m <- rv$mo
        if (is.null(m)) return(NULL)
        row <- m[m$mo_id == m_id, ]
        if (nrow(row) == 0) return(NULL)
        paste0(row$mo_short_name[1], " \u2014 ", rv$indicator_b)
      }
    })

    # === РЕНДЕРИНГ ГРАФИКА ===
    output$chart <- renderPlotly({
      data <- chart_data()

      if (is.null(data) || nrow(data) == 0) {
        msg <- if (type == "epi") {
          "Кликните на район на карте"
        } else {
          "Кликните на МО на карте"
        }
        return(create_empty_chart(msg))
      }

      # Определяем цвет линии: голубой для эпидемиологии, оранжевый для скрининга
      color_idx <- if (type == "epi") 1 else 2

      # Строим график с уникальным source для plotly_click
      create_time_series_chart(
        data      = data,
        title     = chart_title(),
        xlab      = "Период",
        ylab      = "Значение",
        color_idx = color_idx,
        source_id = plotly_source
      )
    })

    # === ОБРАБОТКА КЛИКА ПО ТОЧКЕ ГРАФИКА ===
    # При клике по точке → обновляем rv$selected_period_a/b,
    # что фильтрует карту по выбранному периоду.
    observe({
      click <- event_data("plotly_click", source = plotly_source)
      if (is.null(click)) return()

      # Извлекаем период из x-координаты (формат "YYYY" или "YYYY-MM")
      period_str <- as.character(click$x[1])
      parts <- strsplit(period_str, "-")[[1]]

      yr <- suppressWarnings(as.integer(parts[1]))
      mo <- if (length(parts) >= 2) suppressWarnings(as.integer(parts[2])) else NA_integer_

      if (is.na(yr)) return()

      period <- list(year = yr, month = mo)

      if (type == "epi") {
        # Если уже выбран тот же период — сбрасываем фильтр
        current <- rv$selected_period_a
        if (!is.null(current) && identical(current$year, yr) && identical(current$month, mo)) {
          rv$selected_period_a <- NULL
        } else {
          rv$selected_period_a <- period
        }
      } else {
        current <- rv$selected_period_b
        if (!is.null(current) && identical(current$year, yr) && identical(current$month, mo)) {
          rv$selected_period_b <- NULL
        } else {
          rv$selected_period_b <- period
        }
      }
    })

  })
}
