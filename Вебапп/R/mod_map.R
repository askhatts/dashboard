# ============================================================
# МОДУЛЬ: ИНТЕРАКТИВНАЯ КАРТА (Двухуровневая)
# ============================================================
# Реализует главную карту дашборда с двумя слоями:
#   Слой 1 (нижний): Хороплет районов — заливка по квартилям
#                     выбранного показателя А (эпидемиология)
#   Слой 2 (верхний): Круги МО — размер по квартилям
#                     выбранного показателя Б (скрининги),
#                     цвет статичный (оранжевый)
#
# Особенности:
#   - leafletProxy() для обновления слоёв без перерисовки карты
#   - Клик по району → rv$selected_district_id
#   - Клик по МО → rv$selected_mo_id
#   - МО без данных скрининга НЕ отображаются на карте
#   - При пустых данных слои и легенды корректно очищаются
#   - Поддержка фильтрации по периоду (rv$selected_period_a/b)
# ============================================================

# === UI МОДУЛЯ ===
mod_map_ui <- function(id) {
  ns <- NS(id)

  # Карта на полную ширину/высоту контейнера
  leafletOutput(ns("map"), width = "100%", height = "100%")
}

# === SERVER МОДУЛЯ ===
mod_map_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # === ИНИЦИАЛИЗАЦИЯ БАЗОВОЙ КАРТЫ ===
    # Создаётся ОДИН РАЗ при запуске. Далее обновляется через leafletProxy().
    output$map <- renderLeaflet({
      leaflet() %>%
        # Тёмная подложка (стиль ArcGIS)
        addProviderTiles(providers$CartoDB.DarkMatter, group = "Тёмная") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Светлая") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Спутник") %>%
        # Начальный вид: Абайская область
        setView(lng = DEFAULT_MAP_LNG, lat = DEFAULT_MAP_LAT, zoom = DEFAULT_MAP_ZOOM) %>%
        # Переключатель слоёв
        addLayersControl(
          baseGroups = c("Тёмная", "Светлая", "Спутник"),
          overlayGroups = c("Районы", "Медорганизации"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>%
        # Кнопка полного экрана
        addFullscreenControl(position = "topright")
    })

    # === РЕАКТИВНЫЕ ДАННЫЕ ДЛЯ ХОРОПЛЕТА РАЙОНОВ ===
    # Вычисляет значения показателя А для каждого района,
    # агрегируя данные эпидемиологии (сумма по всем годам/месяцам).
    districts_map_data <- reactive({
      req(rv$districts, rv$indicator_a)

      d <- rv$districts
      epi <- rv$epi

      if (is.null(epi) || nrow(epi) == 0 || rv$indicator_a == "") {
        # Если данных нет — возвращаем районы с нулями
        d$value <- 0
        return(d)
      }

      # Фильтруем по выбранному показателю
      epi_filtered <- epi %>%
        filter(indicator == rv$indicator_a)

      # Если выбран конкретный период — фильтруем по нему
      sel_period <- rv$selected_period_a
      if (!is.null(sel_period)) {
        epi_filtered <- epi_filtered %>%
          filter(year == sel_period$year, month == sel_period$month)
      }

      # Суммируем по районам
      epi_agg <- epi_filtered %>%
        group_by(district_id) %>%
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

      # Присоединяем к sf-объекту районов
      d <- d %>%
        left_join(epi_agg, by = "district_id") %>%
        mutate(value = ifelse(is.na(value), 0, value))

      return(d)
    })

    # === РЕАКТИВНЫЕ ДАННЫЕ ДЛЯ МО ===
    # Все МО с координатами (для иконок).
    all_mo_data <- reactive({
      m <- rv$mo
      if (is.null(m) || nrow(m) == 0) return(NULL)
      m %>% filter(!is.na(latitude) & !is.na(longitude))
    })

    # Данные скрининга по МО (для кругов поверх иконок).
    mo_map_data <- reactive({
      m <- rv$mo
      if (is.null(m) || nrow(m) == 0) return(NULL)

      scr <- rv$scr
      ind_b <- rv$indicator_b

      if (is.null(scr) || nrow(scr) == 0 || is.null(ind_b) || ind_b == "") {
        return(NULL)
      }

      scr_filtered <- scr %>% filter(indicator == ind_b)

      sel_period <- rv$selected_period_b
      if (!is.null(sel_period)) {
        scr_filtered <- scr_filtered %>%
          filter(year == sel_period$year, month == sel_period$month)
      }

      scr_agg <- scr_filtered %>%
        group_by(mo_id) %>%
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        filter(value > 0)

      if (nrow(scr_agg) == 0) return(NULL)

      m <- m %>%
        inner_join(scr_agg, by = "mo_id") %>%
        filter(!is.na(latitude) & !is.na(longitude))

      if (nrow(m) == 0) return(NULL)

      breaks <- compute_quartile_breaks(m$value)
      m$quartile <- assign_quartile_class(m$value, breaks)
      m$radius <- MO_CIRCLE_SIZES[m$quartile]

      return(m)
    })

    # === ОБНОВЛЕНИЕ СЛОЯ РАЙОНОВ (хороплет) ===
    # Срабатывает при изменении показателя А или данных эпидемиологии
    observe({
      data <- districts_map_data()

      # Если данных нет — очищаем слой и легенду
      if (is.null(data) || nrow(data) == 0) {
        leafletProxy(ns("map")) %>%
          clearGroup("Районы") %>%
          removeControl("legend_districts")
        return()
      }

      # Вычисляем квартильные границы для палитры
      breaks <- compute_quartile_breaks(data$value)

      # Создаём палитру: 4 цвета по квартилям
      pal <- colorBin(
        palette  = CHOROPLETH_COLORS,
        domain   = data$value,
        bins     = breaks,
        na.color = "#3a3a3a"
      )

      # Формируем содержимое попапов (HTML)
      popups <- paste0(
        "<div style='font-family:Inter,sans-serif;min-width:180px;'>",
        "<strong style='color:#00d2ff;font-size:14px;'>", data$district_name_ru, "</strong><br>",
        "<span style='color:#a0a0a0;font-size:11px;'>", data$district_type, "</span>",
        "<hr style='border-color:#30305a;margin:6px 0;'>",
        "<span style='color:#a0a0a0;font-size:11px;'>", rv$indicator_a, ":</span><br>",
        "<span style='font-size:18px;font-weight:700;color:#ffffff;'>",
        round(data$value, 1), "</span>",
        "</div>"
      )

      # Обновляем карту через proxy (без полной перерисовки)
      leafletProxy(ns("map")) %>%
        clearGroup("Районы") %>%
        removeControl("legend_districts") %>%
        addPolygons(
          data         = data,
          group        = "Районы",
          fillColor    = ~pal(value),
          weight       = 1.5,
          opacity      = 1,
          color        = "rgba(255,255,255,0.25)",
          fillOpacity  = 0.7,
          popup        = popups,
          label        = ~paste0(district_name_ru, ": ", round(value, 1)),
          layerId      = ~paste0("district_", district_id),
          highlightOptions = highlightOptions(
            weight = 3, color = "#00d2ff", fillOpacity = 0.9, bringToFront = FALSE
          )
        ) %>%
        addLegend(
          position = "topright",
          pal      = pal,
          values   = data$value,
          title    = rv$indicator_a,
          layerId  = "legend_districts",
          opacity  = 0.9
        )
    })

    # === ОБНОВЛЕНИЕ СЛОЯ МО ===
    # Иконки больниц для ВСЕХ МО с координатами.
    # Круги поверх для МО с данными скрининга (размер по квартилям).
    observe({
      all_mo <- all_mo_data()

      proxy <- leafletProxy(ns("map")) %>%
        clearGroup("Медорганизации") %>%
        removeControl("legend_mo")

      # Если нет МО — выходим
      if (is.null(all_mo) || nrow(all_mo) == 0) return()

      # Иконка больницы (HTML divIcon — маленький крестик)
      hospital_icon <- makeIcon(
        iconUrl = NULL,
        iconWidth = 14, iconHeight = 14
      )

      mo_with_scr <- mo_map_data()
      mo_with_scr_ids <- if (is.null(mo_with_scr) || nrow(mo_with_scr) == 0) integer(0) else mo_with_scr$mo_id

      # Попапы для всех МО (базовая информация)
      base_popups <- paste0(
        "<div style='font-family:Inter,sans-serif;min-width:180px;'>",
        "<strong style='color:#ff6b35;font-size:14px;'>", all_mo$mo_short_name, "</strong><br>",
        "<span style='color:#a0a0a0;font-size:11px;'>", all_mo$mo_type,
        " (", all_mo$ownership, ")</span><br>",
        "<span style='color:#6c757d;font-size:11px;'>Район: ", all_mo$district_name_ru, "</span><br>",
        "<span style='color:#a0a0a0;font-size:11px;'>Скрининг: ",
        ifelse(all_mo$mo_id %in% mo_with_scr_ids, "есть данные", "&#9679; нет данных"),
        "</span>",
        "</div>"
      )

      # Добавляем маленькие маркеры-крестики для всех МО
      proxy <- proxy %>%
        addCircleMarkers(
          data        = all_mo,
          group       = "Медорганизации",
          lng         = ~longitude,
          lat         = ~latitude,
          radius      = 3,
          fillColor   = "#b0b7c3",
          fillOpacity = 0.8,
          color       = "#c6ccd8",
          weight      = 1,
          popup       = base_popups,
          label       = ~mo_short_name,
          layerId     = ~paste0("mo_", mo_id)
        )

      # Если есть данные скрининга — круги поверх
      data <- mo_with_scr
      if (!is.null(data) && nrow(data) > 0) {
        scr_popups <- paste0(
          "<div style='font-family:Inter,sans-serif;min-width:200px;'>",
          "<strong style='color:#ff6b35;font-size:14px;'>", data$mo_short_name, "</strong><br>",
          "<span style='color:#a0a0a0;font-size:11px;'>", data$mo_type,
          " (", data$ownership, ")</span><br>",
          "<span style='color:#6c757d;font-size:11px;'>Район: ", data$district_name_ru, "</span>",
          "<hr style='border-color:#30305a;margin:6px 0;'>",
          "<span style='color:#a0a0a0;font-size:11px;'>", rv$indicator_b, ":</span><br>",
          "<span style='font-size:18px;font-weight:700;color:#ffffff;'>",
          round(data$value, 1), "</span>",
          "</div>"
        )

        breaks <- compute_quartile_breaks(data$value)
        size_legend_html <- create_size_legend_html(rv$indicator_b, breaks)

        proxy <- proxy %>%
          addCircleMarkers(
            data        = data,
            group       = "Медорганизации",
            lng         = ~longitude,
            lat         = ~latitude,
            radius      = ~radius,
            fillColor   = MO_CIRCLE_COLOR,
            fillOpacity = 0.85,
            color       = "#ffffff",
            weight      = 1.5,
            popup       = scr_popups,
            label       = ~paste0(mo_short_name, ": ", round(value, 1)),
            layerId     = ~paste0("mo_scr_", mo_id)
          ) %>%
          addControl(
            html     = size_legend_html,
            position = "topright",
            layerId  = "legend_mo"
          )
      }
    })

    # === ОБРАБОТКА КЛИКА ПО РАЙОНУ ===
    # При клике на полигон района — сохраняем его ID в rv
    # для обновления графика А (эпидемиология)
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      if (!is.null(click$id) && grepl("^district_", click$id)) {
        rv$selected_district_id <- as.integer(gsub("district_", "", click$id))
      }
    })

    # === ОБРАБОТКА КЛИКА ПО МО ===
    # При клике на маркер/круг МО — сохраняем его ID в rv
    # для обновления графика Б (скрининг)
    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      if (!is.null(click$id)) {
        # Обрабатываем оба формата: "mo_123" и "mo_scr_123"
        if (grepl("^mo_scr_", click$id)) {
          rv$selected_mo_id <- as.integer(gsub("mo_scr_", "", click$id))
        } else if (grepl("^mo_", click$id)) {
          rv$selected_mo_id <- as.integer(gsub("mo_", "", click$id))
        }
      }
    })

  })
}
