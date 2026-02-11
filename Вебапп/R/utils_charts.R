# ============================================================
# УТИЛИТЫ ДЛЯ ПОСТРОЕНИЯ ГРАФИКОВ (Plotly)
# ============================================================
# Все графики стилизованы под тёмную тему ArcGIS Dashboard:
# тёмный фон, светлые подписи, контрастные цвета линий.
# ============================================================

# === ЦВЕТА ЛИНИЙ ГРАФИКОВ ===
CHART_LINE_COLORS <- c(
  "#00d2ff",  # Голубой (основной)
  "#ff6b35",  # Оранжевый
  "#00e676",  # Зелёный
  "#ff5252",  # Красный
  "#ffc107",  # Жёлтый
  "#bb86fc",  # Фиолетовый
  "#03dac6",  # Бирюзовый
  "#cf6679"   # Розовый
)

# === ТЁМНАЯ ТЕМА ДЛЯ PLOTLY ===
# Применяет единый стиль к любому plotly-объекту:
# тёмный фон, белые подписи осей, убранные лишние элементы.
plotly_dark_layout <- function(p, title = NULL, xlab = NULL, ylab = NULL) {
  p %>%
    plotly::layout(
      title = list(
        text = title,
        font = list(color = "#e0e0e0", size = 14, family = "Inter, Arial, sans-serif"),
        x = 0.02, xanchor = "left"
      ),
      xaxis = list(
        title = list(text = xlab, font = list(color = "#a0a0a0", size = 11)),
        tickfont   = list(color = "#a0a0a0", size = 10),
        gridcolor  = "#2a2d45",
        zerolinecolor = "#2a2d45",
        tickangle  = -45
      ),
      yaxis = list(
        title = list(text = ylab, font = list(color = "#a0a0a0", size = 11)),
        tickfont   = list(color = "#a0a0a0", size = 10),
        gridcolor  = "#2a2d45",
        zerolinecolor = "#2a2d45"
      ),
      paper_bgcolor = "rgba(0,0,0,0)",  # Прозрачный фон (виден CSS-фон панели)
      plot_bgcolor  = "rgba(26,26,46,0.5)",
      font = list(color = "#e0e0e0", family = "Inter, Arial, sans-serif"),
      margin = list(l = 50, r = 20, t = 40, b = 60),
      hovermode = "x unified",
      legend = list(
        font = list(color = "#e0e0e0", size = 10),
        bgcolor = "rgba(26,26,46,0.8)",
        bordercolor = "#30305a",
        borderwidth = 1
      )
    ) %>%
    plotly::config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d"),
      locale = "ru"
    )
}

# === ФОРМАТИРОВАНИЕ ПЕРИОДА ===
# Создаёт текстовую метку периода из года и месяца.
# Если месяц = NA, возвращает только год ("2024").
# Иначе — "2024-03" для сортируемого формата.
format_period <- function(year, month) {
  ifelse(
    is.na(month),
    as.character(year),
    paste0(year, "-", sprintf("%02d", month))
  )
}

# === ЛИНЕЙНЫЙ ГРАФИК ВРЕМЕННОЙ ДИНАМИКИ ===
# Универсальная функция для построения линейного графика.
# Используется в обоих зонах графиков (A: эпидемиология, B: скрининг).
#
# Параметры:
#   data      — data.frame с колонками: year, month, indicator, value
#   title     — заголовок графика
#   xlab/ylab — подписи осей
#   color_idx — индекс цвета из палитры CHART_LINE_COLORS
create_time_series_chart <- function(data, title = NULL, xlab = "Период",
                                     ylab = "Значение", color_idx = 1) {
  if (is.null(data) || nrow(data) == 0) {
    # Если данных нет — показываем плейсхолдер
    return(create_empty_chart("Нет данных для отображения"))
  }

  # Формируем метку периода и сортируем
  data <- data %>%
    dplyr::mutate(period = format_period(year, month)) %>%
    dplyr::arrange(year, month)

  # Определяем цвет линии
  line_color <- CHART_LINE_COLORS[((color_idx - 1) %% length(CHART_LINE_COLORS)) + 1]

  # Проверяем, есть ли несколько показателей (для легенды)
  indicators <- unique(data$indicator)

  if (length(indicators) <= 1) {
    # Один показатель — одна линия
    p <- plotly::plot_ly(
      data, x = ~period, y = ~value,
      type = "scatter", mode = "lines+markers",
      line = list(color = line_color, width = 2.5),
      marker = list(color = line_color, size = 6, line = list(color = "white", width = 1)),
      name = if (length(indicators) == 1) indicators[1] else "Значение",
      hovertemplate = paste0(
        "<b>%{x}</b><br>",
        "Значение: %{y:.1f}<extra></extra>"
      )
    )
  } else {
    # Несколько показателей — несколько линий с разными цветами
    p <- plotly::plot_ly()
    for (i in seq_along(indicators)) {
      ind_data <- data %>% dplyr::filter(indicator == indicators[i])
      lc <- CHART_LINE_COLORS[((i - 1) %% length(CHART_LINE_COLORS)) + 1]

      p <- p %>% plotly::add_trace(
        data = ind_data, x = ~period, y = ~value,
        type = "scatter", mode = "lines+markers",
        line = list(color = lc, width = 2),
        marker = list(color = lc, size = 5),
        name = indicators[i],
        hovertemplate = paste0(
          "<b>", indicators[i], "</b><br>",
          "%{x}: %{y:.1f}<extra></extra>"
        )
      )
    }
  }

  # Применяем тёмную тему
  plotly_dark_layout(p, title = title, xlab = xlab, ylab = ylab)
}

# === ПУСТОЙ ГРАФИК-ПЛЕЙСХОЛДЕР ===
# Показывается, когда данных нет или ничего не выбрано на карте.
# Содержит текстовую аннотацию по центру.
create_empty_chart <- function(message = "Кликните на карте для выбора") {
  plotly::plot_ly() %>%
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(26,26,46,0.3)",
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(
        list(
          text = message,
          xref = "paper", yref = "paper",
          x = 0.5, y = 0.5,
          xanchor = "center", yanchor = "middle",
          showarrow = FALSE,
          font = list(color = "#6c757d", size = 14, family = "Inter, Arial, sans-serif")
        )
      )
    ) %>%
    plotly::config(displayModeBar = FALSE)
}
