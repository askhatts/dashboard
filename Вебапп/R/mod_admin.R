# ============================================================
# МОДУЛЬ: АДМИН-ПАНЕЛЬ
# ============================================================
# Защищённая паролем панель для:
#   1. Загрузки данных из Excel (эпид, скрининг, координаты, шейпфайл)
#   2. Ручного ввода эпидемиологии по районам
#   3. Редактирования данных в DT (inline cell editing)
#   4. CRUD операций напрямую в SQLite
#   5. Скачивания шаблонов Excel
#   6. Справочник районов и МО
# ============================================================

# === UI МОДУЛЯ ===
mod_admin_ui <- function(id) {
  ns <- NS(id)

  # ИСПРАВЛЕНИЕ: используем style="display:none;" вместо hidden()
  # hidden() добавляет класс shinyjs-hide с display:none!important,
  # который не перебивается jQuery .show()
  div(id = ns("admin_panel"), class = "admin-overlay", style = "display: none;",
    div(class = "admin-content", style = "position: relative; max-height: 90vh; overflow-y: auto;",
      actionButton(ns("close_admin"), label = NULL, icon = icon("times"),
                   class = "admin-close"),

      h3("Панель администратора",
         style = "color: #ffffff; margin-bottom: 16px;"),

      tabsetPanel(
        id = ns("admin_tabs"),
        type = "pills",

        # === ВКЛАДКА 1: ЗАГРУЗКА ДАННЫХ ===
        tabPanel(
          title = "Загрузка",
          icon  = icon("upload"),
          div(style = "padding-top: 16px;",
            fluidRow(
              # --- Эпидемиология ---
              column(6,
                h4("Эпидемиология", style = "color: #00d2ff;"),
                p("Excel: Район | Год | Показатель1 | Показатель2 | ...",
                  style = "color: #6c757d; font-size: 12px;"),
                fileInput(ns("file_epi"), "Excel (эпидемиология):",
                          accept = c(".xlsx", ".xls")),
                fluidRow(
                  column(6, numericInput(ns("epi_year_upload"), "Год (если нет в файле):",
                                         value = as.integer(format(Sys.Date(), "%Y")),
                                         min = 2020, max = 2035)),
                  column(6, numericInput(ns("epi_month_upload"), "Месяц (если нет):",
                                         value = NA, min = 1, max = 12))
                ),
                actionButton(ns("btn_import_epi"), "Импортировать эпидемиологию",
                             icon = icon("upload"), class = "btn-primary btn-sm",
                             style = "margin-top: 4px;")
              ),

              # --- Скрининг ---
              column(6,
                h4("Скрининг", style = "color: #ff6b35;"),
                p("Excel: МО | Год | Месяц | Показатели... ИЛИ пациентский формат",
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

        # === ВКЛАДКА 2: ЭПИДЕМИОЛОГИЯ (ручной ввод + редактирование) ===
        tabPanel(
          title = "Эпидемиология",
          icon  = icon("chart-line"),
          div(style = "padding-top: 16px;",
            h4("Ручной ввод эпидемиологических данных", style = "color: #00d2ff;"),
            fluidRow(
              column(3, selectInput(ns("epi_district"), "Район:", choices = NULL)),
              column(2, numericInput(ns("epi_year"), "Год:",
                                     value = as.integer(format(Sys.Date(), "%Y")),
                                     min = 2020, max = 2035)),
              column(2, numericInput(ns("epi_month"), "Месяц:",
                                     value = NA, min = 1, max = 12)),
              column(3, textInput(ns("epi_indicator"), "Показатель:",
                                   placeholder = "Заболеваемость")),
              column(2, numericInput(ns("epi_value"), "Значение:", value = NA))
            ),
            actionButton(ns("btn_epi_add"), "Добавить запись",
                         icon = icon("plus"), class = "btn-success btn-sm"),

            tags$hr(style = "border-color: #30305a;"),

            h4("Данные эпидемиологии", style = "color: #00d2ff;"),
            p("Кликните по ячейке для редактирования. Изменения сохраняются в SQL автоматически.",
              style = "color: #6c757d; font-size: 12px;"),
            DTOutput(ns("table_epi_edit")),

            fluidRow(
              column(6,
                actionButton(ns("btn_delete_epi"), "Удалить выбранные строки",
                             icon = icon("trash"), class = "btn-danger btn-sm",
                             style = "margin-top: 8px;")
              ),
              column(6,
                actionButton(ns("btn_clear_epi"), "Очистить всё",
                             icon = icon("trash-alt"), class = "btn-outline-danger btn-sm",
                             style = "margin-top: 8px;")
              )
            )
          )
        ),

        # === ВКЛАДКА 3: СКРИНИНГ (ручной ввод + редактирование) ===
        tabPanel(
          title = "Скрининг",
          icon  = icon("microscope"),
          div(style = "padding-top: 16px;",
            h4("Ручной ввод данных скрининга", style = "color: #ff6b35;"),
            fluidRow(
              column(3, selectInput(ns("scr_mo"), "Медорганизация:", choices = NULL)),
              column(2, numericInput(ns("scr_year_manual"), "Год:",
                                     value = as.integer(format(Sys.Date(), "%Y")),
                                     min = 2020, max = 2035)),
              column(1, numericInput(ns("scr_month_manual"), "Месяц:",
                                     value = NA, min = 1, max = 12)),
              column(2, textInput(ns("scr_indicator"), "Показатель:",
                                   placeholder = "Начато")),
              column(2, numericInput(ns("scr_value_manual"), "Значение:", value = NA)),
              column(2, selectInput(ns("scr_type_manual"), "Тип:",
                                    choices = c("" = "", "РМЖ" = "РМЖ", "КРР" = "КРР", "РШМ" = "РШМ")))
            ),
            actionButton(ns("btn_scr_add"), "Добавить запись",
                         icon = icon("plus"), class = "btn-success btn-sm"),

            tags$hr(style = "border-color: #30305a;"),

            h4("Данные скрининга", style = "color: #ff6b35;"),
            p("Кликните по ячейке для редактирования. Изменения сохраняются в SQL автоматически.",
              style = "color: #6c757d; font-size: 12px;"),
            DTOutput(ns("table_scr_edit")),

            fluidRow(
              column(6,
                actionButton(ns("btn_delete_scr"), "Удалить выбранные строки",
                             icon = icon("trash"), class = "btn-danger btn-sm",
                             style = "margin-top: 8px;")
              ),
              column(6,
                actionButton(ns("btn_clear_scr"), "Очистить всё",
                             icon = icon("trash-alt"), class = "btn-outline-danger btn-sm",
                             style = "margin-top: 8px;")
              )
            )
          )
        ),

        # === ВКЛАДКА 4: МЕДОРГАНИЗАЦИИ (координаты, тип, район) ===
        tabPanel(
          title = "Медорганизации",
          icon  = icon("hospital"),
          div(style = "padding-top: 16px;",
            h4("Добавить медорганизацию", style = "color: #00e676;"),
            fluidRow(
              column(4, textInput(ns("mo_new_name"), "Название МО:",
                                   placeholder = "КГП на ПХВ 'Поликлиника №1'")),
              column(2, textInput(ns("mo_new_type"), "Тип:", placeholder = "Поликлиника")),
              column(2, numericInput(ns("mo_new_lat"), "Широта:", value = NA)),
              column(2, numericInput(ns("mo_new_lon"), "Долгота:", value = NA)),
              column(2, selectInput(ns("mo_new_district"), "Район:", choices = NULL))
            ),
            actionButton(ns("btn_mo_add"), "Добавить МО",
                         icon = icon("plus"), class = "btn-success btn-sm"),

            tags$hr(style = "border-color: #30305a;"),

            h4("Медорганизации", style = "color: #00e676;"),
            p("Редактируйте координаты, тип, название. Изменения сохраняются в SQL автоматически.",
              style = "color: #6c757d; font-size: 12px;"),
            DTOutput(ns("table_mo_edit")),
            actionButton(ns("btn_delete_mo"), "Деактивировать выбранные МО",
                         icon = icon("trash"), class = "btn-danger btn-sm",
                         style = "margin-top: 8px;")
          )
        ),

        # === ВКЛАДКА 5: РАЙОНЫ (справочник) ===
        tabPanel(
          title = "Районы",
          icon  = icon("map"),
          div(style = "padding-top: 16px;",
            h4("Справочник районов", style = "color: #ffc107;"),
            p("Справочная таблица (только чтение).",
              style = "color: #6c757d; font-size: 12px;"),
            DTOutput(ns("ref_districts"))
          )
        ),

        # === ВКЛАДКА 6: ШАБЛОНЫ ===
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
}

# === SERVER МОДУЛЯ ===
mod_admin_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    is_authorized <- reactiveVal(FALSE)

    # === АВТОРИЗАЦИЯ ===
    observeEvent(rv$trigger_admin, {
      if (is_authorized()) {
        shinyjs::show("admin_panel")
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
        shinyjs::show("admin_panel")
        showNotification("Вход выполнен", type = "message", duration = 3)
      } else {
        showNotification("Неверный пароль!", type = "error", duration = 5)
      }
    })

    # Закрытие админ-панели
    observeEvent(input$close_admin, {
      shinyjs::hide("admin_panel")
    })

    # === ЗАПОЛНЕНИЕ СПИСКОВ ДЛЯ РУЧНОГО ВВОДА ===
    observe({
      d <- rv$districts
      if (!is.null(d) && nrow(d) > 0) {
        df <- sf::st_drop_geometry(d)
        choices <- setNames(df$district_id, df$district_name_ru)
        updateSelectInput(session, "epi_district", choices = choices)
        updateSelectInput(session, "mo_new_district", choices = choices)
      }
    })

    observe({
      m <- rv$mo
      if (!is.null(m) && nrow(m) > 0) {
        choices <- setNames(m$mo_id, m$mo_short_name)
        updateSelectInput(session, "scr_mo", choices = choices)
      }
    })

    # === СПРАВОЧНАЯ ТАБЛИЦА РАЙОНОВ ===
    output$ref_districts <- renderDT({
      d <- rv$districts
      if (is.null(d) || nrow(d) == 0) return(datatable(data.frame(`Нет данных` = ""), rownames = FALSE))
      df <- sf::st_drop_geometry(d)[, c("district_id", "district_pcode", "district_name_ru",
                                          "district_type", "area_km2", "population")]
      datatable(df, options = list(pageLength = 10, scrollX = TRUE, dom = "t"), rownames = FALSE,
                colnames = c("ID", "PCODE", "Район", "Тип", "Площадь км2", "Население"))
    })

    # ============================================
    # === ИМПОРТ ЭПИДЕМИОЛОГИИ ===
    # ============================================
    observeEvent(input$btn_import_epi, {
      req(input$file_epi)
      showModal(modalDialog(
        title = "Подтверждение импорта",
        p("Импортировать данные эпидемиологии из загруженного файла?"),
        p("Данные будут добавлены в базу SQL.", style = "color: #ffc107;"),
        footer = tagList(
          modalButton("Отмена"),
          actionButton(ns("btn_confirm_import_epi"), "Подтвердить", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$btn_confirm_import_epi, {
      removeModal()
      tryCatch({
        raw <- readxl::read_excel(input$file_epi$datapath, guess_max = 5000)
        long_data <- parse_excel_to_long_format(
          data = raw,
          entity_type    = "district",
          reference_data = rv$districts,
          year_override  = input$epi_year_upload,
          month_override = input$epi_month_upload
        )
        n_imported <- insert_epi_from_import(rv$db_conn, long_data)
        log_import(rv$db_conn, "epidemiology", input$file_epi$name,
                   nrow(long_data), n_imported, nrow(long_data) - n_imported, "success")

        # Перезагружаем из SQL
        rv$epi <- load_epidemiology(rv$db_conn)
        showNotification(paste0("Эпидемиология: импортировано ", n_imported, " записей"),
                        type = "message", duration = 8)
      }, error = function(e) {
        showNotification(paste("Ошибка импорта эпидемиологии:", e$message),
                        type = "error", duration = 10)
      })
    })

    # ============================================
    # === ИМПОРТ СКРИНИНГА ===
    # ============================================
    observeEvent(input$btn_import_scr, {
      req(input$file_scr)
      showModal(modalDialog(
        title = "Подтверждение импорта",
        p("Импортировать данные скрининга из загруженного файла?"),
        p("Данные будут добавлены в базу SQL.", style = "color: #ffc107;"),
        footer = tagList(
          modalButton("Отмена"),
          actionButton(ns("btn_confirm_import_scr"), "Подтвердить", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$btn_confirm_import_scr, {
      removeModal()
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

        # screening_type = "" т.к. префикс уже включён в indicator
        # (parse_patient_level_screening добавляет его, а load_screening
        #  конкатенирует screening_type || ' - ' || indicator)
        n_imported <- insert_scr_from_import(rv$db_conn, long_data, "")
        log_import(rv$db_conn, "screening", input$file_scr$name,
                   nrow(long_data), n_imported, nrow(long_data) - n_imported, "success")

        # Перезагружаем из SQL
        rv$scr <- load_screening(rv$db_conn)
        showNotification(paste0("Скрининг: импортировано ", n_imported, " записей"),
                        type = "message", duration = 8)

        unmatched_count <- attr(long_data, "unmatched_mo_count") %||% 0L
        unmatched_names <- attr(long_data, "unmatched_mo_names")
        if (unmatched_count > 0) {
          preview <- ""
          if (!is.null(unmatched_names) && length(unmatched_names) > 0) {
            preview <- paste0(" Примеры: ", paste(utils::head(unmatched_names, 3), collapse = "; "))
          }
          showNotification(
            paste0("Не сопоставлено МО строк: ", unmatched_count, ". Эти строки пропущены.", preview),
            type = "warning", duration = 12
          )
        }
      }, error = function(e) {
        showNotification(paste("Ошибка импорта скрининга:", e$message),
                        type = "error", duration = 10)
      })
    })

    # ============================================
    # === ИМПОРТ КООРДИНАТ МО ===
    # ============================================
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

            mid <- matches$matched_mo_id[j]
            update_mo_field(rv$db_conn, mid, "latitude", lat)
            update_mo_field(rv$db_conn, mid, "longitude", lon)
            updated <- updated + 1
          }

          # Перезагружаем МО из SQL
          rv$mo <- load_mo(rv$db_conn)
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

    # ============================================
    # === ИМПОРТ ШЕЙПФАЙЛА ===
    # ============================================
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

    # ============================================
    # === РУЧНОЙ ВВОД ЭПИДЕМИОЛОГИИ ===
    # ============================================
    observeEvent(input$btn_epi_add, {
      req(input$epi_district, input$epi_year, input$epi_indicator)

      tryCatch({
        district_id <- as.integer(input$epi_district)
        year_val <- as.integer(input$epi_year)
        month_val <- if (is.na(input$epi_month)) NA_integer_ else as.integer(input$epi_month)
        indicator_val <- trimws(input$epi_indicator)
        value_val <- if (is.na(input$epi_value)) NA_real_ else as.numeric(input$epi_value)

        if (nchar(indicator_val) == 0) {
          showNotification("Введите название показателя", type = "warning")
          return()
        }

        save_epi_row(rv$db_conn, district_id, year_val, month_val, indicator_val, value_val)

        # Перезагружаем из SQL
        rv$epi <- load_epidemiology(rv$db_conn)

        showNotification("Запись добавлена", type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("Ошибка:", e$message), type = "error", duration = 8)
      })
    })

    # ============================================
    # === РЕДАКТИРУЕМАЯ ТАБЛИЦА ЭПИДЕМИОЛОГИИ ===
    # ============================================

    # Маппинг столбцов DT → столбцов SQL для эпидемиологии
    epi_col_map <- c("epi_id", "district_id", "data_year", "data_month", "indicator", "value", "import_date")

    output$table_epi_edit <- renderDT({
      data <- rv$epi
      if (is.null(data) || nrow(data) == 0) {
        return(datatable(data.frame(`Нет данных` = ""), rownames = FALSE))
      }
      datatable(data,
        editable = list(target = "cell", disable = list(columns = c(0, 6))),
        selection = "multiple",
        options  = list(scrollX = TRUE, pageLength = 15, order = list(list(0, "desc"))),
        rownames = FALSE,
        colnames = c("ID", "Район ID", "Год", "Месяц", "Показатель", "Значение", "Дата импорта")
      )
    })

    observeEvent(input$table_epi_edit_cell_edit, {
      info <- input$table_epi_edit_cell_edit
      epi <- rv$epi
      if (is.null(epi) || nrow(epi) == 0) return()

      row_idx <- info$row
      col_idx <- info$col + 1  # DT 0-based → R 1-based
      if (row_idx < 1 || row_idx > nrow(epi)) return()
      if (col_idx < 1 || col_idx > length(epi_col_map)) return()

      epi_id <- epi$epi_id[row_idx]
      sql_col <- epi_col_map[col_idx]
      new_value <- info$value

      # Обновляем в SQL
      tryCatch({
        update_epi_cell(rv$db_conn, epi_id, sql_col, new_value)
        # Обновляем в rv
        epi[row_idx, col_idx] <- DT::coerceValue(new_value, epi[row_idx, col_idx])
        rv$epi <- epi
      }, error = function(e) {
        showNotification(paste("Ошибка обновления:", e$message), type = "error")
      })
    })

    # Удаление выбранных строк эпидемиологии
    observeEvent(input$btn_delete_epi, {
      sel <- input$table_epi_edit_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        showNotification("Выберите строки для удаления", type = "warning")
        return()
      }
      showModal(modalDialog(
        title = "Подтверждение удаления",
        p(paste0("Удалить ", length(sel), " выбранных записей эпидемиологии?")),
        footer = tagList(
          modalButton("Отмена"),
          actionButton(ns("btn_confirm_delete_epi"), "Удалить", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$btn_confirm_delete_epi, {
      removeModal()
      sel <- input$table_epi_edit_rows_selected
      epi <- rv$epi
      if (!is.null(sel) && length(sel) > 0 && !is.null(epi) && nrow(epi) > 0) {
        ids_to_delete <- epi$epi_id[sel]
        tryCatch({
          delete_epi_rows(rv$db_conn, ids_to_delete)
          rv$epi <- load_epidemiology(rv$db_conn)
          showNotification(paste0("Удалено ", length(ids_to_delete), " записей"), type = "message")
        }, error = function(e) {
          showNotification(paste("Ошибка удаления:", e$message), type = "error")
        })
      }
    })

    # Очистка всей эпидемиологии
    observeEvent(input$btn_clear_epi, {
      showModal(modalDialog(
        title = "Подтверждение очистки",
        p("Удалить ВСЕ данные эпидемиологии?", style = "color: #ff5252; font-weight: bold;"),
        footer = tagList(
          modalButton("Отмена"),
          actionButton(ns("btn_confirm_clear_epi"), "Очистить всё", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$btn_confirm_clear_epi, {
      removeModal()
      tryCatch({
        DBI::dbExecute(rv$db_conn, "DELETE FROM epidemiology_data")
        rv$epi <- create_empty_epidemiology()
        showNotification("Данные эпидемиологии очищены", type = "warning", duration = 5)
      }, error = function(e) {
        showNotification(paste("Ошибка:", e$message), type = "error")
      })
    })

    # ============================================
    # === РУЧНОЙ ВВОД СКРИНИНГА ===
    # ============================================
    observeEvent(input$btn_scr_add, {
      req(input$scr_mo, input$scr_year_manual, input$scr_indicator)

      tryCatch({
        mo_id_val <- as.integer(input$scr_mo)
        year_val <- as.integer(input$scr_year_manual)
        month_val <- if (is.na(input$scr_month_manual)) NA_integer_ else as.integer(input$scr_month_manual)
        indicator_val <- trimws(input$scr_indicator)
        value_val <- if (is.na(input$scr_value_manual)) NA_real_ else as.numeric(input$scr_value_manual)
        scr_type_val <- if (is.null(input$scr_type_manual)) "" else input$scr_type_manual

        if (nchar(indicator_val) == 0) {
          showNotification("Введите название показателя", type = "warning")
          return()
        }

        save_scr_row(rv$db_conn, mo_id_val, year_val, month_val, scr_type_val, indicator_val, value_val)
        rv$scr <- load_screening(rv$db_conn)
        showNotification("Запись скрининга добавлена", type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("Ошибка:", e$message), type = "error", duration = 8)
      })
    })

    # ============================================
    # === РЕДАКТИРУЕМАЯ ТАБЛИЦА СКРИНИНГА ===
    # ============================================

    scr_col_map <- c("scr_id", "mo_id", "data_year", "data_month", "indicator", "value", "import_date")

    output$table_scr_edit <- renderDT({
      data <- rv$scr
      if (is.null(data) || nrow(data) == 0) {
        return(datatable(data.frame(`Нет данных` = ""), rownames = FALSE))
      }
      datatable(data,
        editable = list(target = "cell", disable = list(columns = c(0, 6))),
        selection = "multiple",
        options  = list(scrollX = TRUE, pageLength = 15, order = list(list(0, "desc"))),
        rownames = FALSE,
        colnames = c("ID", "МО ID", "Год", "Месяц", "Показатель", "Значение", "Дата импорта")
      )
    })

    observeEvent(input$table_scr_edit_cell_edit, {
      info <- input$table_scr_edit_cell_edit
      scr <- rv$scr
      if (is.null(scr) || nrow(scr) == 0) return()

      row_idx <- info$row
      col_idx <- info$col + 1
      if (row_idx < 1 || row_idx > nrow(scr)) return()
      if (col_idx < 1 || col_idx > length(scr_col_map)) return()

      scr_id <- scr$scr_id[row_idx]
      sql_col <- scr_col_map[col_idx]
      new_value <- info$value

      tryCatch({
        update_scr_cell(rv$db_conn, scr_id, sql_col, new_value)
        scr[row_idx, col_idx] <- DT::coerceValue(new_value, scr[row_idx, col_idx])
        rv$scr <- scr
      }, error = function(e) {
        showNotification(paste("Ошибка обновления:", e$message), type = "error")
      })
    })

    # Удаление выбранных строк скрининга
    observeEvent(input$btn_delete_scr, {
      sel <- input$table_scr_edit_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        showNotification("Выберите строки для удаления", type = "warning")
        return()
      }
      showModal(modalDialog(
        title = "Подтверждение удаления",
        p(paste0("Удалить ", length(sel), " выбранных записей скрининга?")),
        footer = tagList(
          modalButton("Отмена"),
          actionButton(ns("btn_confirm_delete_scr"), "Удалить", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$btn_confirm_delete_scr, {
      removeModal()
      sel <- input$table_scr_edit_rows_selected
      scr <- rv$scr
      if (!is.null(sel) && length(sel) > 0 && !is.null(scr) && nrow(scr) > 0) {
        ids_to_delete <- scr$scr_id[sel]
        tryCatch({
          delete_scr_rows(rv$db_conn, ids_to_delete)
          rv$scr <- load_screening(rv$db_conn)
          showNotification(paste0("Удалено ", length(ids_to_delete), " записей"), type = "message")
        }, error = function(e) {
          showNotification(paste("Ошибка удаления:", e$message), type = "error")
        })
      }
    })

    # Очистка всего скрининга
    observeEvent(input$btn_clear_scr, {
      showModal(modalDialog(
        title = "Подтверждение очистки",
        p("Удалить ВСЕ данные скрининга?", style = "color: #ff5252; font-weight: bold;"),
        footer = tagList(
          modalButton("Отмена"),
          actionButton(ns("btn_confirm_clear_scr"), "Очистить всё", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$btn_confirm_clear_scr, {
      removeModal()
      tryCatch({
        DBI::dbExecute(rv$db_conn, "DELETE FROM screening_data")
        rv$scr <- create_empty_screening()
        showNotification("Данные скрининга очищены", type = "warning", duration = 5)
      }, error = function(e) {
        showNotification(paste("Ошибка:", e$message), type = "error")
      })
    })

    # ============================================
    # === РУЧНОЙ ВВОД МЕДОРГАНИЗАЦИИ ===
    # ============================================
    observeEvent(input$btn_mo_add, {
      req(input$mo_new_name)
      tryCatch({
        mo_name <- trimws(input$mo_new_name)
        if (nchar(mo_name) == 0) {
          showNotification("Введите название МО", type = "warning")
          return()
        }
        mo_type <- if (is.null(input$mo_new_type) || trimws(input$mo_new_type) == "") "Поликлиника" else trimws(input$mo_new_type)
        lat <- if (is.na(input$mo_new_lat)) NA_real_ else as.numeric(input$mo_new_lat)
        lon <- if (is.na(input$mo_new_lon)) NA_real_ else as.numeric(input$mo_new_lon)
        dist_id <- as.integer(input$mo_new_district)

        add_mo(rv$db_conn, mo_name, mo_type, lat, lon, dist_id)
        rv$mo <- load_mo(rv$db_conn)
        showNotification("Медорганизация добавлена", type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("Ошибка:", e$message), type = "error", duration = 8)
      })
    })

    # ============================================
    # === РЕДАКТИРУЕМАЯ ТАБЛИЦА МЕДОРГАНИЗАЦИЙ ===
    # ============================================

    # Столбцы МО: mo_id, mo_name, mo_short_name, mo_name_normalized, mo_type, ownership, latitude, longitude, district_id, district_name_ru
    mo_editable_cols <- c("mo_name", "mo_short_name", "mo_type", "ownership", "latitude", "longitude")

    output$table_mo_edit <- renderDT({
      data <- rv$mo
      if (is.null(data) || nrow(data) == 0) {
        return(datatable(data.frame(`Нет данных` = ""), rownames = FALSE))
      }
      # Показываем только нужные колонки для редактирования
      display_df <- data[, c("mo_id", "mo_short_name", "mo_type", "ownership",
                              "latitude", "longitude", "district_name_ru")]
      datatable(display_df,
        editable = list(target = "cell", disable = list(columns = c(0, 6))),
        selection = "multiple",
        options  = list(scrollX = TRUE, pageLength = 15),
        rownames = FALSE,
        colnames = c("ID", "Название МО", "Тип", "Форма собст.", "Широта", "Долгота", "Район")
      )
    })

    observeEvent(input$table_mo_edit_cell_edit, {
      info <- input$table_mo_edit_cell_edit
      mo <- rv$mo
      if (is.null(mo) || nrow(mo) == 0) return()

      row_idx <- info$row
      if (row_idx < 1 || row_idx > nrow(mo)) return()

      # Столбцы display: mo_id, mo_short_name, mo_type, ownership, latitude, longitude, district_name_ru
      display_to_sql <- c("mo_id", "mo_short_name", "mo_type", "ownership", "latitude", "longitude", "district_name_ru")
      col_idx <- info$col + 1
      if (col_idx < 1 || col_idx > length(display_to_sql)) return()

      sql_field <- display_to_sql[col_idx]
      mo_id <- mo$mo_id[row_idx]
      new_value <- info$value

      tryCatch({
        update_mo_field(rv$db_conn, mo_id, sql_field, new_value)
        # Перезагружаем МО из SQL
        rv$mo <- load_mo(rv$db_conn)
      }, error = function(e) {
        showNotification(paste("Ошибка обновления МО:", e$message), type = "error")
      })
    })

    observeEvent(input$btn_delete_mo, {
      sel <- input$table_mo_edit_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        showNotification("Выберите МО для деактивации", type = "warning")
        return()
      }

      showModal(modalDialog(
        title = "Подтверждение",
        p(paste0("Деактивировать ", length(sel), " выбранных МО?")),
        footer = tagList(
          modalButton("Отмена"),
          actionButton(ns("btn_confirm_delete_mo"), "Деактивировать", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$btn_confirm_delete_mo, {
      removeModal()
      sel <- input$table_mo_edit_rows_selected
      mo <- rv$mo
      if (!is.null(sel) && length(sel) > 0 && !is.null(mo) && nrow(mo) > 0) {
        ids_to_deactivate <- mo$mo_id[sel]
        tryCatch({
          deactivate_mo_rows(rv$db_conn, ids_to_deactivate)
          rv$mo <- load_mo(rv$db_conn)
          showNotification(paste0("Деактивировано ", length(ids_to_deactivate), " МО"), type = "message")
        }, error = function(e) {
          showNotification(paste("Ошибка деактивации МО:", e$message), type = "error")
        })
      }
    })

    # ============================================
    # === ШАБЛОНЫ ===
    # ============================================
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
