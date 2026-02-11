# ============================================================
# МОДУЛЬ: ГРАФИКИ ДИНАМИКИ (Зоны А и Б)
# ============================================================
# Универсальный модуль, который создаётся дважды:
#   - Зона А (type = "epi"): линейный график эпидемиологии
#     для района, выбранного кликом на карте
#   - Зона Б (type = "scr"): линейный график скрининга
#     для МО, выбранной кликом на карте
#
# Графики строятся в тёмной теме plotly и показывают
# временную динамику выбранного показателя.
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

    # === РЕАКТИВНЫЕ ДАННЫЕ ДЛЯ ГРАФИКА ===
    # Фильтруют данные по выбранному объекту (район/МО)
    # и выбранному показателю (indicator_a/indicator_b)
    chart_data <- reactive({
      if (type == "epi") {
        # --- Зона А: Эпидемиология по выбранному району ---
        district_id <- rv$selected_district_id
        indicator   <- rv$indicator_a
        epi         <- rv$epi

        # Если район не выбран — показываем плейсхолдер
        if (is.null(district_id) || is.null(indicator) || indicator == "") {
          return(NULL)
        }
        if (is.null(epi) || nrow(epi) == 0) return(NULL)

        # Фильтруем данные по району и показателю
        filtered <- epi %>%
          filter(
            district_id == !!district_id,
            indicator == !!indicator
          ) %>%
          arrange(year, month)

        return(filtered)

      } else {
        # --- Зона Б: Скрининг по выбранной МО ---
        mo_id     <- rv$selected_mo_id
        indicator <- rv$indicator_b
        scr       <- rv$scr

        if (is.null(mo_id) || is.null(indicator) || indicator == "") {
          return(NULL)
        }
        if (is.null(scr) || nrow(scr) == 0) return(NULL)

        # Фильтруем данные по МО и показателю
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
    # Показывает название выбранного объекта
    chart_title <- reactive({
      if (type == "epi") {
        d_id <- rv$selected_district_id
        if (is.null(d_id)) return(NULL)
        d <- rv$districts
        if (is.null(d)) return(NULL)
        row <- d[d$district_id == d_id, ]
        if (nrow(row) == 0) return(NULL)
        paste0(row$district_name_ru[1], " — ", rv$indicator_a)
      } else {
        m_id <- rv$selected_mo_id
        if (is.null(m_id)) return(NULL)
        m <- rv$mo
        if (is.null(m)) return(NULL)
        row <- m[m$mo_id == m_id, ]
        if (nrow(row) == 0) return(NULL)
        paste0(row$mo_short_name[1], " — ", rv$indicator_b)
      }
    })

    # === РЕНДЕРИНГ ГРАФИКА ===
    output$chart <- renderPlotly({
      data <- chart_data()

      if (is.null(data) || nrow(data) == 0) {
        # Плейсхолдер с подсказкой
        msg <- if (type == "epi") {
          "Кликните на район на карте"
        } else {
          "Кликните на МО на карте"
        }
        return(create_empty_chart(msg))
      }

      # Определяем цвет линии: голубой для эпидемиологии, оранжевый для скрининга
      color_idx <- if (type == "epi") 1 else 2

      # Строим линейный график через утилиту
      create_time_series_chart(
        data      = data,
        title     = chart_title(),
        xlab      = "Период",
        ylab      = "Значение",
        color_idx = color_idx
      )
    })

  })
}
