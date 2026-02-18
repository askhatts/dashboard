# ============================================================
# SERVER.R — Корневая серверная логика
# ============================================================
# Здесь:
#   1. Создаётся per-session SQLite соединение
#   2. Инициализируются общие реактивные данные (rv)
#   3. Вызываются все модули с передачей rv
#   4. Обрабатываются глобальные события (fullscreen, admin trigger)
# ============================================================

function(input, output, session) {

  # === PER-SESSION SQLite СОЕДИНЕНИЕ ===
  db_conn <- get_db_connection()
  session$onSessionEnded(function() {
    tryCatch(DBI::dbDisconnect(db_conn), error = function(e) NULL)
  })

  # === ОБЩЕЕ РЕАКТИВНОЕ ХРАНИЛИЩЕ ===
  # Все модули работают с одним и тем же rv, что обеспечивает
  # связь между картой, графиками, админкой и селекторами.
  rv <- reactiveValues(
    # --- SQLite соединение ---
    db_conn   = db_conn,

    # --- Пространственные данные ---
    districts = INIT_DISTRICTS,    # sf-объект районов (из global.R)
    mo        = INIT_MO,          # data.frame МО с координатами

    # --- Атрибутивные данные (длинный формат) ---
    epi       = INIT_EPIDEMIOLOGY, # Эпидемиология
    scr       = INIT_SCREENING,    # Скрининг

    # --- Выбранные показатели (из mod_controls) ---
    indicator_a = NULL,            # Показатель А (эпидемиология → цвет районов)
    indicator_b = NULL,            # Показатель Б (скрининг → размер кругов МО)

    # --- Выбранные объекты на карте (из mod_map) ---
    selected_district_id = NULL,   # ID кликнутого района
    selected_mo_id       = NULL,   # ID кликнутой МО

    # --- Выбранные периоды на графиках (из mod_charts) ---
    selected_period_a = NULL,      # list(year, month) из клика на графике А
    selected_period_b = NULL,      # list(year, month) из клика на графике Б

    # --- Триггер открытия админки ---
    trigger_admin = NULL
  )

  # === ВЫЗОВ МОДУЛЕЙ ===
  # Каждый модуль получает ссылку на rv и может читать/записывать данные.

  # Панель управления (селекторы показателей A и B)
  mod_controls_server("controls", rv)

  # Карта (двухслойная: хороплет районов + круги МО)
  mod_map_server("map", rv)

  # Зона графиков А: Эпидемиология (по выбранному району)
  mod_charts_server("chart_epi", rv, type = "epi")

  # Зона графиков Б: Скрининг (по выбранной МО)
  mod_charts_server("chart_scr", rv, type = "scr")

  # Админ-панель (загрузка, редактирование, сохранение)
  mod_admin_server("admin", rv)

  # === ТРИГГЕР ОТКРЫТИЯ АДМИНКИ ===
  # При клике на gear-иконку в хедере —
  # обновляем rv$trigger_admin, что запускает логику авторизации в mod_admin
  observeEvent(input$btn_admin_trigger, {
    rv$trigger_admin <- Sys.time()  # Уникальное значение для каждого клика
  })

  # === ПОЛНОЭКРАННЫЕ КНОПКИ ДЛЯ ГРАФИКОВ ===
  # При клике на кнопку fullscreen —
  # отправляем JS-сообщение для переключения CSS-класса

  # Зона А (Эпидемиология)
  observeEvent(input$fs_chart_a, {
    session$sendCustomMessage("toggleFullscreen", list(panelId = "panel_chart_a"))
  })

  # Зона Б (Скрининг)
  observeEvent(input$fs_chart_b, {
    session$sendCustomMessage("toggleFullscreen", list(panelId = "panel_chart_b"))
  })

}
