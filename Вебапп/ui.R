# ============================================================
# UI.R — Корневой пользовательский интерфейс
# ============================================================
# Реализует макет в стиле ArcGIS Dashboard:
#   - Тонкий заголовок с названием и кнопкой админки
#   - Боковая панель с селекторами показателей и KPI
#   - Основная зона: карта на весь экран
#   - Плавающие панели графиков (A и B) поверх карты
# ============================================================

fluidPage(
  # === ТЕМА И ЗАВИСИМОСТИ ===
  theme = app_theme,
  useShinyjs(),          # Для show/hide панелей
  tags$head(
    # Подключаем Google Font "Inter" для профессионального вида
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"
    ),
    # Подключаем кастомные CSS и JS
    tags$link(rel = "stylesheet", href = "custom.css"),
    tags$script(src = "custom.js")
  ),

  # === ЗАГОЛОВОК (HEADER BAR) ===
  div(class = "header-bar",
    # Логотип и название
    div(class = "logo-title",
      # Иконка (вместо логотипа)
      tags$span(
        icon("chart-line"),
        style = "color: #00d2ff; font-size: 20px; margin-right: 8px;"
      ),
      tags$span(class = "title", "Онкологическая аналитика"),
      tags$span(class = "subtitle", "Абайская область")
    ),
    # Кнопка админки (gear icon)
    actionButton(
      "btn_admin_trigger",
      label = NULL,
      icon  = icon("gear"),
      class = "admin-trigger"
    )
  ),

  # === ОСНОВНОЙ КОНТЕНТ ===
  div(class = "main-content",

    # --- Боковая панель (селекторы + KPI) ---
    div(class = "sidebar-panel",
      mod_controls_ui("controls")
    ),

    # --- Контейнер карты (занимает всю оставшуюся ширину) ---
    div(class = "map-container",

      # Карта (100% ширины и высоты контейнера)
      mod_map_ui("map"),

      # --- Плавающая панель: Зона А (Эпидемиология) ---
      div(id = "panel_chart_a", class = "floating-panel chart-panel-a",
        div(class = "panel-header",
          tags$span(class = "panel-title", "Эпидемиология"),
          tags$span(class = "panel-subtitle", "(клик по району)"),
          # Кнопка полного экрана
          actionButton("fs_chart_a", label = NULL, icon = icon("expand"),
                       class = "btn-fullscreen")
        ),
        mod_charts_ui("chart_epi")
      ),

      # --- Плавающая панель: Зона Б (Скрининг) ---
      div(id = "panel_chart_b", class = "floating-panel chart-panel-b",
        div(class = "panel-header",
          tags$span(class = "panel-title", "Скрининг"),
          tags$span(class = "panel-subtitle", "(клик по МО)"),
          actionButton("fs_chart_b", label = NULL, icon = icon("expand"),
                       class = "btn-fullscreen")
        ),
        mod_charts_ui("chart_scr")
      )
    )
  ),

  # === АДМИН-ПАНЕЛЬ (скрытая, поверх основного контента) ===
  mod_admin_ui("admin")
)
