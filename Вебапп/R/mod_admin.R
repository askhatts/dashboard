# ============================================================
# МОДУЛЬ: АДМИН-ПАНЕЛЬ
# ============================================================
# Защищённая паролем панель для:
#   1. Загрузки данных из Excel (по отдельности: эпид, скрининг, координаты, шейпфайл)
#   2. Поддержка пациентского формата скрининга (per-patient → агрегация)
#   3. Редактирования данных в табличном интерфейсе (inline)
#   4. Сохранения изменений в .rds файлы
#   5. Скачивания шаблонов Excel
#   6. Справочник ID районов и МО
# ============================================================

# === UI МОДУЛЯ ===
mod_admin_ui <- function(id) {
  ns <- NS(id)

  hidden(
    div(id = ns("admin_panel"), class = "admin-overlay",
      div(class = "admin-content", style = "position: relative;",
        actionButton(ns("close_admin"), label = NULL, icon = icon("times"),
                     class = "admin-close"),

        h3("Панель администратора",
           style = "color: #ffffff; margin-bottom: 16px;"),

        tabsetPanel(
          id = ns("admin_tabs"),
          type = "pills",

          # === ВКЛАДКА 1: ЗАГРУЗКА ДАННЫХ ===
          tabPanel(
            title = "Загрузка данных",
            icon  = icon("upload"),
            div(style = "padding-top: 16px;",

              # --- Справочник ID (сворачиваемый) ---
              tags$details(style = "margin-bottom: 16px; border: 1px solid #30305a; border-radius: 6px; padding: 8px 12px;",
                tags$summary(style = "color: #ffc107; cursor: pointer; font-weight: 600;",
                  icon("info-circle"), " Справочник ID районов и медорганизаций"
                ),
                div(style = "margin-top: 12px;",
                  fluidRow(
                    column(6,
                      h5("Районы", style = "color: #00d2ff;"),
                      DTOutput(ns("ref_districts"))
                    ),
                    column(6,
                      h5("Медорганизации", style = "color: #ff6b35;"),
                      DTOutput(ns("ref_mo"))
                    )
                  )
                )
              ),

              fluidRow(
                # --- Эпидемиология ---
                column(6,
                  h4("Эпидемиология", style = "color: #00d2ff;"),
                  p("Excel: Район | Год | Показатель1 | Показатель2 | ...",
                    style = "color: #6c757d; font-size: 12px;"),
                  fileInput(ns("file_epi"), "Excel (эпидемиология):",
                            accept = c(".xlsx", ".xls")),
                  fluidRow(
                    column(6, numericInput(ns("epi_year"), "Год (если нет в файле):",
                                           value = as.integer(format(Sys.Date(), "%Y")),
                                           min = 2020, max = 2035)),
                    column(6, numericInput(ns("epi_month"), "Месяц (если нет):",
                                           value = NA, min = 1, max = 12))
                  ),
                  actionButton(ns("btn_import_epi"), "Импортировать эпидемиологию",
                               icon = icon("upload"), class = "btn-primary btn-sm",
                               style = "margin-top: 4px;")
                ),

                # --- Скрининг ---
                column(6,
                  h4("Скрининг", style = "color: #ff6b35;"),
                  p("Excel: МО | Год | Месяц | Показатели... ИЛИ пациентский формат (ФИО, ИИН, Дата_начала...)",
                    style = "color: #6c757d; font-size: 12px;"),
                  fileInput(ns("file_scr"), "Excel (скрининг):",
                            accept = c(".xlsx", ".xls")),
                  fluidRow(
                    column(4, selectInput(ns("scr_type"), "Тип скрининга:",
                                          choices = c("РМЖ" = "РМЖ", "КРР" = "КРР", "РШМ" = "РШМ", "Без префикса" = ""),
                                          width = "100%")),
                    column(4, numericInput(ns("scr_year"), "Год:",
                                           value = as.integer(format(Sys.Date(), "%Y")),
                                           min = 2020, max = 2035)),
                    column(4, numericInput(ns("scr_month"), "Месяц:",
                                           value = NA, min = 1, max = 12))
                  ),
                  actionButton(ns("btn_import_scr"), "Импортировать скрининг",
                               icon = icon("upload"), class = "btn-primary btn-sm",
                               style = "margin-top: 4px;")
                )
              ),

              tags$hr(style = "border-color: #30305a;"),

              fluidRow(
                # --- Координаты МО ---
                column(6,
                  h4("Координаты МО", style = "color: #00e676;"),
                  p("Excel: МО | Широта | Долгота",
                    style = "color: #6c757d; font-size: 12px;"),
                  fileInput(ns("file_mo_coords"), "Excel (координаты):",
                            accept = c(".xlsx", ".xls")),
                  actionButton(ns("btn_import_coords"), "Импортировать координаты",
                               icon = icon("upload"), class = "btn-primary btn-sm",
                               style = "margin-top: 4px;")
                ),

                # --- Шейпфайл ---
                column(6,
                  h4("Шейпфайл районов", style = "color: #ffc107;"),
                  p("Загрузите все 5 файлов: .shp, .shx, .dbf, .prj, .cpg",
                    style = "color: #6c757d; font-size: 12px;"),
                  fileInput(ns("file_shapefile"), "Файлы шейпа:",
                            accept = c(".shp", ".shx", ".dbf", ".prj", ".cpg"),
                            multiple = TRUE),
                  actionButton(ns("btn_import_shp"), "Импортировать шейпфайл",
                               icon = icon("upload"), class = "btn-primary btn-sm",
                               style = "margin-top: 4px;")
                )
              ),

              # Предпросмотр загруженных данных
              div(style = "margin-top: 16px;",
                uiOutput(ns("preview_ui"))
              )
            )
          ),

          # === ВКЛАДКА 2: РЕДАКТИРОВАНИЕ ДАННЫХ ===
          tabPanel(
            title = "Редактирование",
            icon  = icon("edit"),
            div(style = "padding-top: 16px;",
              h4("Эпидемиология", style = "color: #00d2ff;"),
              DTOutput(ns("table_epi_edit")),

              tags$hr(style = "border-color: #30305a;"),

              h4("Скрининг", style = "color: #ff6b35;"),
              DTOutput(ns("table_scr_edit")),

              tags$hr(style = "border-color: #30305a;"),

              h4("Медорганизации", style = "color: #00e676;"),
              DTOutput(ns("table_mo_edit")),

              tags$hr(style = "border-color: #30305a;"),

              fluidRow(
                column(4,
                  actionButton(ns("btn_save"), "Сохранить все изменения",
                               icon = icon("save"), class = "btn-success btn-lg",
                               style = "margin-top: 8px;")
                ),
                column(4,
                  actionButton(ns("btn_clear_epi"), "Очистить эпидемиологию",
                               icon = icon("trash"), class = "btn-danger btn-sm",
                               style = "margin-top: 12px;")
                ),
                column(4,
                  actionButton(ns("btn_clear_scr"), "Очистить скрининг",
                               icon = icon("trash"), class = "btn-danger btn-sm",
                               style = "margin-top: 12px;")
                )
              )
            )
          ),

          # === ВКЛАДКА 3: ШАБЛОНЫ ===
          tabPanel(
            title = "Шаблоны",
            icon  = icon("download"),
            div(style = "padding-top: 16px;",
              h4("Скачать шаблоны Excel", style = "color: #ffffff;"),
              p("Используйте эти шаблоны для подготовки данных к загрузке.",
                style = "color: #a0a0a0;"),

              tags$br(),
              fluidRow(
                column(3,
                  div(class = "info-box", style = "text-align: center; padding: 20px;",
                    icon("table", style = "font-size: 24px; color: #00d2ff;"),
                    tags$br(), tags$br(),
                    strong("Эпидемиология", style = "color: #e0e0e0;"),
                    tags$br(),
                    span("Район, Год, Показатели", style = "color: #6c757d; font-size: 11px;"),
                    tags$br(), tags$br(),
                    downloadButton(ns("dl_tpl_epi"), "Скачать",
                                   class = "btn-download")
                  )
                ),
                column(3,
                  div(class = "info-box", style = "text-align: center; padding: 20px;",
                    icon("hospital", style = "font-size: 24px; color: #ff6b35;"),
                    tags$br(), tags$br(),
                    strong("Скрининг (агрег.)", style = "color: #e0e0e0;"),
                    tags$br(),
                    span("МО, Год, Месяц, Показатели", style = "color: #6c757d; font-size: 11px;"),
                    tags$br(), tags$br(),
                    downloadButton(ns("dl_tpl_scr"), "Скачать",
                                   class = "btn-download")
                  )
                ),
                column(3,
                  div(class = "info-box", style = "text-align: center; padding: 20px;",
                    icon("user", style = "font-size: 24px; color: #bb86fc;"),
                    tags$br(), tags$br(),
                    strong("Скрининг (пациент.)", style = "color: #e0e0e0;"),
                    tags$br(),
                    span("МО, ФИО, ИИН, Дата, Показатели", style = "color: #6c757d; font-size: 11px;"),
                    tags$br(), tags$br(),
                    downloadButton(ns("dl_tpl_scr_patient"), "Скачать",
                                   class = "btn-download")
                  )
                ),
                column(3,
                  div(class = "info-box", style = "text-align: center; padding: 20px;",
                    icon("map-marker-alt", style = "font-size: 24px; color: #00e676;"),
                    tags$br(), tags$br(),
                    strong("Координаты МО", style = "color: #e0e0e0;"),
                    tags$br(),
                    span("МО, Широта, Долгота", style = "color: #6c757d; font-size: 11px;"),
                    tags$br(), tags$br(),
                    downloadButton(ns("dl_tpl_coords"), "Скачать",
                                   class = "btn-download")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

# === SERVER МОДУЛЯ ===
mod_admin_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    is_authorized <- reactiveVal(FALSE)

    # === АВТОРИЗАЦИЯ ===
    observeEvent(rv$trigger_admin, {
      if (is_authorized()) {
        shinyjs::runjs(paste0("$('#", ns("admin_panel"), "').show();"))
      } else {
        showModal(modalDialog(
          title = "Авторизация администратора",
          passwordInput(ns("admin_password"), "Введите пароль:"),
          footer = tagList(
            modalButton("Отмена"),
            actionButton(ns("btn_login"), "Войти", class = "btn-primary")
          ),
          easyClose = TRUE
        ))
      }
    })

    observeEvent(input$btn_login, {
      entered_hash <- digest::digest(input$admin_password, algo = "sha256")
      if (entered_hash == ADMIN_PASSWORD_HASH) {
        is_authorized(TRUE)
        removeModal()
        shinyjs::runjs(paste0("$('#", ns("admin_panel"), "').show();"))
        showNotification("Вход выполнен", type = "message", duration = 3)
      } else {
        showNotification("Неверный пароль!", type = "error", duration = 5)
      }
    })

    # Закрытие админ-панели
    observeEvent(input$close_admin, {
      shinyjs::runjs(paste0("$('#", ns("admin_panel"), "').hide();"))
    })

    # === СПРАВОЧНЫЕ ТАБЛИЦЫ ===
    output$ref_districts <- renderDT({
      d <- rv$districts
      if (is.null(d) || nrow(d) == 0) return(datatable(data.frame(`Нет данных` = ""), rownames = FALSE))
      df <- st_drop_geometry(d)[, c("district_id", "district_pcode", "district_name_ru")]
      datatable(df, options = list(pageLength = 10, scrollY = "200px", dom = "t"), rownames = FALSE,
                colnames = c("ID", "PCODE", "Район"))
    })

    output$ref_mo <- renderDT({
      m <- rv$mo
      if (is.null(m) || nrow(m) == 0) return(datatable(data.frame(`Нет данных` = ""), rownames = FALSE))
      df <- m[, c("mo_id", "mo_short_name", "district_name_ru")]
      datatable(df, options = list(pageLength = 10, scrollY = "200px", dom = "tp"), rownames = FALSE,
                colnames = c("ID", "МО", "Район"))
    })

    # === ИМПОРТ ЭПИДЕМИОЛОГИИ ===
    observeEvent(input$btn_import_epi, {
      req(input$file_epi)
      tryCatch({
        raw <- readxl::read_excel(input$file_epi$datapath, guess_max = 5000)

        long_data <- parse_excel_to_long_format(
          data = raw,
          entity_type    = "district",
          reference_data = rv$districts,
          year_override  = input$epi_year,
          month_override = input$epi_month
        )

        existing <- rv$epi
        max_id <- if (nrow(existing) > 0) max(existing$epi_id) else 0

        new_epi <- long_data %>%
          dplyr::filter(!is.na(entity_id)) %>%
          dplyr::transmute(
            epi_id      = seq(max_id + 1, max_id + dplyr::n()),
            district_id = entity_id,
            year        = year,
            month       = month,
            indicator   = indicator,
            value       = value,
            import_date = import_date
          )

        rv$epi <- dplyr::bind_rows(existing, new_epi)
        save_epidemiology(rv$epi)
        showNotification(paste0("Эпидемиология: импортировано ", nrow(new_epi), " записей"),
                        type = "message", duration = 8)
      }, error = function(e) {
        showNotification(paste("Ошибка импорта эпидемиологии:", e$message),
                        type = "error", duration = 10)
      })
    })

    # === ИМПОРТ СКРИНИНГА ===
    observeEvent(input$btn_import_scr, {
      req(input$file_scr)
      tryCatch({
        raw <- readxl::read_excel(input$file_scr$datapath, guess_max = 10000)

        # Авто-определение формата: пациентский или агрегированный
        if (is_patient_level_format(names(raw))) {
          long_data <- parse_patient_level_screening(
            data = raw,
            reference_mo = rv$mo,
            scr_type_prefix = input$scr_type,
            year_override = input$scr_year,
            month_override = input$scr_month
          )
        } else {
          long_data <- parse_excel_to_long_format(
            data = raw,
            entity_type    = "mo",
            reference_data = rv$mo,
            year_override  = input$scr_year,
            month_override = input$scr_month
          )
        }

        existing <- rv$scr
        max_id <- if (nrow(existing) > 0) max(existing$scr_id) else 0

        new_scr <- long_data %>%
          dplyr::filter(!is.na(entity_id)) %>%
          dplyr::transmute(
            scr_id      = seq(max_id + 1, max_id + dplyr::n()),
            mo_id       = entity_id,
            year        = year,
            month       = month,
            indicator   = indicator,
            value       = value,
            import_date = import_date
          )

        rv$scr <- dplyr::bind_rows(existing, new_scr)
        save_screening(rv$scr)
        showNotification(paste0("Скрининг: импортировано ", nrow(new_scr), " записей"),
                        type = "message", duration = 8)
      }, error = function(e) {
        showNotification(paste("Ошибка импорта скрининга:", e$message),
                        type = "error", duration = 10)
      })
    })

    # === ИМПОРТ КООРДИНАТ МО ===
    observeEvent(input$btn_import_coords, {
      req(input$file_mo_coords)
      tryCatch({
        raw <- readxl::read_excel(input$file_mo_coords$datapath)
        detected <- auto_detect_columns(names(raw), "mo")

        if (!is.null(detected$entity_col) && !is.null(detected$lat_col) && !is.null(detected$lon_col)) {
          mo_current <- rv$mo
          matches <- match_mo_names(as.character(raw[[detected$entity_col]]), mo_current)
          updated <- 0

          for (j in 1:nrow(raw)) {
            if (is.na(matches$matched_mo_id[j])) next
            lat <- suppressWarnings(as.numeric(raw[[detected$lat_col]][j]))
            lon <- suppressWarnings(as.numeric(raw[[detected$lon_col]][j]))
            if (is.na(lat) || is.na(lon)) next

            idx <- which(mo_current$mo_id == matches$matched_mo_id[j])
            if (length(idx) > 0) {
              mo_current$latitude[idx]  <- lat
              mo_current$longitude[idx] <- lon
              updated <- updated + 1
            }
          }

          rv$mo <- mo_current
          save_mo(rv$mo)
          showNotification(paste0("Координаты: обновлено ", updated, " МО"),
                          type = "message", duration = 8)
        } else {
          showNotification("Не найдены необходимые колонки (МО, Широта, Долгота)",
                          type = "error", duration = 8)
        }
      }, error = function(e) {
        showNotification(paste("Ошибка импорта координат:", e$message),
                        type = "error", duration = 10)
      })
    })

    # === ИМПОРТ ШЕЙПФАЙЛА ===
    observeEvent(input$btn_import_shp, {
      req(input$file_shapefile)
      tryCatch({
        tmpdir <- tempdir()
        files <- input$file_shapefile
        shp_file <- NULL

        for (k in 1:nrow(files)) {
          ext <- tools::file_ext(files$name[k])
          new_path <- file.path(tmpdir, files$name[k])
          file.copy(files$datapath[k], new_path, overwrite = TRUE)
          if (ext == "shp") shp_file <- new_path
        }

        if (!is.null(shp_file)) {
          shp_data <- sf::st_read(shp_file, quiet = TRUE)
          abai <- shp_data %>%
            dplyr::filter(ADM1_EN == "Abay Region") %>%
            sf::st_transform(4326)

          if (nrow(abai) > 0) {
            current <- rv$districts
            # Сопоставляем по PCODE если есть
            if ("ADM2_PCODE" %in% names(abai) && "district_pcode" %in% names(current)) {
              for (j in 1:nrow(abai)) {
                idx <- which(current$district_pcode == abai$ADM2_PCODE[j])
                if (length(idx) > 0) {
                  sf::st_geometry(current)[idx] <- abai$geometry[j]
                }
              }
            } else {
              current <- sf::st_sf(
                current %>% sf::st_drop_geometry(),
                geometry = abai$geometry,
                crs = 4326
              )
            }
            rv$districts <- current
            save_districts(rv$districts)
            showNotification(paste0("Шейпфайл: обновлено ", nrow(abai), " районов"),
                            type = "message", duration = 8)
          }
        }
      }, error = function(e) {
        showNotification(paste("Ошибка импорта шейпфайла:", e$message),
                        type = "error", duration = 10)
      })
    })

    # === ПРЕДПРОСМОТР ===
    output$preview_ui <- renderUI({
      if (!is.null(input$file_epi) || !is.null(input$file_scr)) {
        tagList(
          h4("Предпросмотр", style = "color: #ffffff;"),
          DTOutput(ns("preview_table"))
        )
      }
    })

    output$preview_table <- renderDT({
      if (!is.null(input$file_scr)) {
        data <- tryCatch(readxl::read_excel(input$file_scr$datapath, n_max = 50), error = function(e) NULL)
      } else if (!is.null(input$file_epi)) {
        data <- tryCatch(readxl::read_excel(input$file_epi$datapath, n_max = 50), error = function(e) NULL)
      } else {
        data <- NULL
      }

      if (is.null(data)) return(datatable(data.frame(`Файл не загружен` = ""), rownames = FALSE))
      datatable(data, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })

    # === РЕДАКТИРУЕМЫЕ ТАБЛИЦЫ ===
    output$table_epi_edit <- renderDT({
      data <- rv$epi
      if (is.null(data) || nrow(data) == 0) {
        return(datatable(data.frame(`Нет данных` = ""), rownames = FALSE))
      }
      data_display <- tail(data, 100)
      datatable(data_display,
        editable = list(target = "cell", disable = list(columns = c(0))),
        options  = list(scrollX = TRUE, pageLength = 10),
        rownames = FALSE
      )
    })

    observeEvent(input$table_epi_edit_cell_edit, {
      info <- input$table_epi_edit_cell_edit
      epi <- rv$epi
      n <- nrow(epi)
      display_n <- min(n, 100)
      real_row <- n - display_n + info$row
      if (real_row > 0 && real_row <= n) {
        epi[real_row, info$col + 1] <- DT::coerceValue(info$value, epi[real_row, info$col + 1])
        rv$epi <- epi
      }
    })

    output$table_scr_edit <- renderDT({
      data <- rv$scr
      if (is.null(data) || nrow(data) == 0) {
        return(datatable(data.frame(`Нет данных` = ""), rownames = FALSE))
      }
      data_display <- tail(data, 100)
      datatable(data_display,
        editable = list(target = "cell", disable = list(columns = c(0))),
        options  = list(scrollX = TRUE, pageLength = 10),
        rownames = FALSE
      )
    })

    observeEvent(input$table_scr_edit_cell_edit, {
      info <- input$table_scr_edit_cell_edit
      scr <- rv$scr
      n <- nrow(scr)
      display_n <- min(n, 100)
      real_row <- n - display_n + info$row
      if (real_row > 0 && real_row <= n) {
        scr[real_row, info$col + 1] <- DT::coerceValue(info$value, scr[real_row, info$col + 1])
        rv$scr <- scr
      }
    })

    output$table_mo_edit <- renderDT({
      data <- rv$mo
      if (is.null(data) || nrow(data) == 0) {
        return(datatable(data.frame(`Нет данных` = ""), rownames = FALSE))
      }
      datatable(data,
        editable = list(target = "cell", disable = list(columns = c(0))),
        options  = list(scrollX = TRUE, pageLength = 15),
        rownames = FALSE
      )
    })

    observeEvent(input$table_mo_edit_cell_edit, {
      info <- input$table_mo_edit_cell_edit
      mo <- rv$mo
      if (info$row > 0 && info$row <= nrow(mo)) {
        mo[info$row, info$col + 1] <- DT::coerceValue(info$value, mo[info$row, info$col + 1])
        rv$mo <- mo
      }
    })

    # === СОХРАНЕНИЕ ===
    observeEvent(input$btn_save, {
      tryCatch({
        save_epidemiology(rv$epi)
        save_screening(rv$scr)
        save_mo(rv$mo)
        showNotification("Все данные сохранены!", type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste("Ошибка сохранения:", e$message),
                        type = "error", duration = 10)
      })
    })

    # === ОЧИСТКА ДАННЫХ ===
    observeEvent(input$btn_clear_epi, {
      rv$epi <- create_empty_epidemiology()
      save_epidemiology(rv$epi)
      showNotification("Данные эпидемиологии очищены", type = "warning", duration = 5)
    })

    observeEvent(input$btn_clear_scr, {
      rv$scr <- create_empty_screening()
      save_screening(rv$scr)
      showNotification("Данные скрининга очищены", type = "warning", duration = 5)
    })

    # === ШАБЛОНЫ ===
    output$dl_tpl_epi <- downloadHandler(
      filename = function() "template_epidemiology.xlsx",
      content  = function(file) {
        tpl <- data.frame(
          `Район`              = c("Семей", "Абайский район", "Аягозский район"),
          `Год`                = c(2024, 2024, 2024),
          `Заболеваемость`     = c(45.2, 32.1, 28.5),
          `Смертность`         = c(12.1, 8.4, 7.2),
          `Ранняя диагностика (%)` = c(55.3, 48.7, 52.1),
          `Запущенность (%)`   = c(22.1, 28.5, 25.3),
          `5-лет. выживаемость (%)` = c(62.4, 58.1, 60.2),
          check.names = FALSE
        )
        writexl::write_xlsx(tpl, file)
      }
    )

    output$dl_tpl_scr <- downloadHandler(
      filename = function() "template_screening.xlsx",
      content  = function(file) {
        tpl <- data.frame(
          `МО`                  = c("Поликлиника №1", "Поликлиника №2"),
          `Год`                 = c(2024, 2024),
          `Месяц`              = c(1, 1),
          `РМЖ - Начато`       = c(150, 120),
          `РМЖ - Завершено`    = c(140, 115),
          `РМЖ - Рак выявлен`  = c(3, 2),
          `КРР - Начато`       = c(200, 180),
          `КРР - Завершено`    = c(190, 170),
          `КРР - Рак выявлен`  = c(4, 3),
          check.names = FALSE
        )
        writexl::write_xlsx(tpl, file)
      }
    )

    output$dl_tpl_scr_patient <- downloadHandler(
      filename = function() "template_screening_patient.xlsx",
      content  = function(file) {
        tpl <- data.frame(
          `МО_начавшая_осмотр` = c("КГП на ПХВ 'Поликлиника №1 г. Семей' УЗ ОА"),
          `Участок_прикрепления` = c("Участок 1"),
          `ФИО` = c("Иванова А.Б."),
          `ИИН` = c("800101123456"),
          `Пол` = c("Ж"),
          `Возраст` = c(44),
          `Дата_начала` = c("2024-03-15"),
          `Требует_вмешательства` = c(0),
          `Дата_окончания` = c("2024-03-20"),
          `Рак_молочной_железы` = c(0),
          `Трепанобиопсия_всего` = c(1),
          check.names = FALSE
        )
        writexl::write_xlsx(tpl, file)
      }
    )

    output$dl_tpl_coords <- downloadHandler(
      filename = function() "template_mo_coords.xlsx",
      content  = function(file) {
        tpl <- data.frame(
          `МО`      = c("КГП на ПХВ 'Поликлиника №1 г. Семей' УЗ ОА",
                        "КГП на ПХВ 'Поликлиника №2 г. Семей' УЗ ОА"),
          `Широта`  = c(50.4116, 50.4205),
          `Долгота` = c(80.2442, 80.2615),
          check.names = FALSE
        )
        writexl::write_xlsx(tpl, file)
      }
    )

  })
}
