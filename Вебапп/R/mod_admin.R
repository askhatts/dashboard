# ============================================================
# МОДУЛЬ: АДМИН-ПАНЕЛЬ
# ============================================================
# Защищённая паролем панель для:
#   1. Загрузки данных из Excel (эпидемиология, скрининг, координаты МО)
#   2. Загрузки шейпфайлов районов
#   3. Редактирования данных в табличном интерфейсе (inline)
#   4. Сохранения изменений в .rds файлы
#   5. Скачивания шаблонов Excel
# ============================================================

# === UI МОДУЛЯ ===
mod_admin_ui <- function(id) {
  ns <- NS(id)

  # Скрытая панель (показывается после авторизации)
  hidden(
    div(id = ns("admin_panel"), class = "admin-overlay",
      div(class = "admin-content", style = "position: relative;",
        # Кнопка закрытия
        actionButton(ns("close_admin"), label = NULL, icon = icon("times"),
                     class = "admin-close"),

        h3("Панель администратора",
           style = "color: #ffffff; margin-bottom: 16px;"),

        # Вкладки
        tabsetPanel(
          id = ns("admin_tabs"),
          type = "pills",

          # === ВКЛАДКА 1: ЗАГРУЗКА ДАННЫХ ===
          tabPanel(
            title = "Загрузка данных",
            icon  = icon("upload"),
            div(style = "padding-top: 16px;",

              fluidRow(
                # Эпидемиология
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
                  )
                ),

                # Скрининг
                column(6,
                  h4("Скрининг", style = "color: #ff6b35;"),
                  p("Excel: МО | Год | Месяц | Показатель1 | Показатель2 | ...",
                    style = "color: #6c757d; font-size: 12px;"),
                  fileInput(ns("file_scr"), "Excel (скрининг):",
                            accept = c(".xlsx", ".xls")),
                  fluidRow(
                    column(6, numericInput(ns("scr_year"), "Год (если нет в файле):",
                                           value = as.integer(format(Sys.Date(), "%Y")),
                                           min = 2020, max = 2035)),
                    column(6, numericInput(ns("scr_month"), "Месяц (если нет):",
                                           value = NA, min = 1, max = 12))
                  )
                )
              ),

              tags$hr(style = "border-color: #30305a;"),

              fluidRow(
                # Координаты МО
                column(6,
                  h4("Координаты МО", style = "color: #00e676;"),
                  p("Excel: МО | Широта | Долгота",
                    style = "color: #6c757d; font-size: 12px;"),
                  fileInput(ns("file_mo_coords"), "Excel (координаты):",
                            accept = c(".xlsx", ".xls"))
                ),

                # Шейпфайл районов
                column(6,
                  h4("Шейпфайл районов", style = "color: #ffc107;"),
                  p("Загрузите все 5 файлов: .shp, .shx, .dbf, .prj, .cpg",
                    style = "color: #6c757d; font-size: 12px;"),
                  fileInput(ns("file_shapefile"), "Файлы шейпа:",
                            accept = c(".shp", ".shx", ".dbf", ".prj", ".cpg"),
                            multiple = TRUE)
                )
              ),

              tags$hr(style = "border-color: #30305a;"),

              # Кнопка импорта
              actionButton(ns("btn_import"), "Импортировать все загруженные файлы",
                           icon = icon("upload"), class = "btn-primary btn-lg",
                           style = "margin-top: 8px;"),

              # Статус импорта
              div(style = "margin-top: 12px;",
                uiOutput(ns("import_status"))
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

              # Кнопка сохранения
              actionButton(ns("btn_save"), "Сохранить все изменения",
                           icon = icon("save"), class = "btn-success btn-lg",
                           style = "margin-top: 8px;")
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
                column(4,
                  div(class = "info-box", style = "text-align: center; padding: 20px;",
                    icon("table", style = "font-size: 24px; color: #00d2ff;"),
                    tags$br(), tags$br(),
                    strong("Эпидемиология", style = "color: #e0e0e0;"),
                    tags$br(),
                    span("Район, Год, Показатели...", style = "color: #6c757d; font-size: 11px;"),
                    tags$br(), tags$br(),
                    downloadButton(ns("dl_tpl_epi"), "Скачать",
                                   class = "btn-download")
                  )
                ),
                column(4,
                  div(class = "info-box", style = "text-align: center; padding: 20px;",
                    icon("hospital", style = "font-size: 24px; color: #ff6b35;"),
                    tags$br(), tags$br(),
                    strong("Скрининг", style = "color: #e0e0e0;"),
                    tags$br(),
                    span("МО, Год, Месяц, Показатели...", style = "color: #6c757d; font-size: 11px;"),
                    tags$br(), tags$br(),
                    downloadButton(ns("dl_tpl_scr"), "Скачать",
                                   class = "btn-download")
                  )
                ),
                column(4,
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

    # Флаг авторизации
    is_authorized <- reactiveVal(FALSE)

    # === АВТОРИЗАЦИЯ ===
    # Показ модального окна при открытии админки
    observeEvent(rv$trigger_admin, {
      if (is_authorized()) {
        # Если уже авторизован — просто показываем панель
        shinyjs::show("admin_panel")
      } else {
        # Показываем модальное окно с паролем
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

    # Проверка пароля
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

    # === ИМПОРТ ДАННЫХ ===
    observeEvent(input$btn_import, {
      imported <- list()

      # --- Импорт эпидемиологии ---
      if (!is.null(input$file_epi)) {
        tryCatch({
          raw <- read_excel(input$file_epi$datapath, guess_max = 5000)

          # Конвертируем в длинный формат
          long_data <- parse_excel_to_long_format(
            data = raw,
            entity_type    = "district",
            reference_data = rv$districts,
            year_override  = input$epi_year,
            month_override = input$epi_month
          )

          # Присваиваем ID и переименовываем колонки
          existing <- rv$epi
          max_id <- if (nrow(existing) > 0) max(existing$epi_id) else 0

          new_epi <- long_data %>%
            filter(!is.na(entity_id)) %>%
            transmute(
              epi_id      = seq(max_id + 1, max_id + n()),
              district_id = entity_id,
              year        = year,
              month       = month,
              indicator   = indicator,
              value       = value,
              import_date = import_date
            )

          rv$epi <- bind_rows(existing, new_epi)
          save_epidemiology(rv$epi)
          imported$epi <- nrow(new_epi)
        }, error = function(e) {
          showNotification(paste("Ошибка импорта эпидемиологии:", e$message),
                          type = "error", duration = 10)
        })
      }

      # --- Импорт скрининга ---
      if (!is.null(input$file_scr)) {
        tryCatch({
          raw <- read_excel(input$file_scr$datapath, guess_max = 5000)

          long_data <- parse_excel_to_long_format(
            data = raw,
            entity_type    = "mo",
            reference_data = rv$mo,
            year_override  = input$scr_year,
            month_override = input$scr_month
          )

          existing <- rv$scr
          max_id <- if (nrow(existing) > 0) max(existing$scr_id) else 0

          new_scr <- long_data %>%
            filter(!is.na(entity_id)) %>%
            transmute(
              scr_id      = seq(max_id + 1, max_id + n()),
              mo_id       = entity_id,
              year        = year,
              month       = month,
              indicator   = indicator,
              value       = value,
              import_date = import_date
            )

          rv$scr <- bind_rows(existing, new_scr)
          save_screening(rv$scr)
          imported$scr <- nrow(new_scr)
        }, error = function(e) {
          showNotification(paste("Ошибка импорта скрининга:", e$message),
                          type = "error", duration = 10)
        })
      }

      # --- Импорт координат МО ---
      if (!is.null(input$file_mo_coords)) {
        tryCatch({
          raw <- read_excel(input$file_mo_coords$datapath)
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
            imported$coords <- updated
          }
        }, error = function(e) {
          showNotification(paste("Ошибка импорта координат:", e$message),
                          type = "error", duration = 10)
        })
      }

      # --- Импорт шейпфайла ---
      if (!is.null(input$file_shapefile)) {
        tryCatch({
          # Копируем все загруженные файлы во временную директорию
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
            shp_data <- st_read(shp_file, quiet = TRUE)
            # Фильтруем Абайскую область
            abai <- shp_data %>%
              filter(ADM1_EN == "Abay Region") %>%
              st_transform(4326)

            if (nrow(abai) > 0) {
              # Обновляем районы (сохраняя существующие ID)
              # Простая замена геометрии для существующих
              rv$districts <- st_sf(
                rv$districts %>% st_drop_geometry(),
                geometry = abai$geometry,
                crs = 4326
              )
              save_districts(rv$districts)
              imported$shp <- nrow(abai)
            }
          }
        }, error = function(e) {
          showNotification(paste("Ошибка импорта шейпфайла:", e$message),
                          type = "error", duration = 10)
        })
      }

      # Итоговое уведомление
      if (length(imported) > 0) {
        msgs <- c()
        if (!is.null(imported$epi))    msgs <- c(msgs, paste0("Эпидемиология: ", imported$epi, " записей"))
        if (!is.null(imported$scr))    msgs <- c(msgs, paste0("Скрининг: ", imported$scr, " записей"))
        if (!is.null(imported$coords)) msgs <- c(msgs, paste0("Координаты: ", imported$coords, " МО"))
        if (!is.null(imported$shp))    msgs <- c(msgs, paste0("Шейпфайл: ", imported$shp, " районов"))
        showNotification(paste("Импорт завершён:\n", paste(msgs, collapse = "\n")),
                        type = "message", duration = 10)
      } else {
        showNotification("Нет файлов для импорта", type = "warning", duration = 5)
      }
    })

    # === ПРЕДПРОСМОТР ЗАГРУЖЕННЫХ ФАЙЛОВ ===
    output$preview_ui <- renderUI({
      if (!is.null(input$file_epi) || !is.null(input$file_scr)) {
        tagList(
          h4("Предпросмотр", style = "color: #ffffff;"),
          DTOutput(ns("preview_table"))
        )
      }
    })

    output$preview_table <- renderDT({
      # Показываем последний загруженный файл
      if (!is.null(input$file_scr)) {
        data <- tryCatch(read_excel(input$file_scr$datapath, n_max = 50), error = function(e) NULL)
      } else if (!is.null(input$file_epi)) {
        data <- tryCatch(read_excel(input$file_epi$datapath, n_max = 50), error = function(e) NULL)
      } else {
        data <- NULL
      }

      if (is.null(data)) return(datatable(data.frame(`Файл не загружен` = ""), rownames = FALSE))

      datatable(data, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })

    # === СТАТУС ИМПОРТА ===
    output$import_status <- renderUI({
      files_loaded <- c()
      if (!is.null(input$file_epi))       files_loaded <- c(files_loaded, "Эпидемиология")
      if (!is.null(input$file_scr))       files_loaded <- c(files_loaded, "Скрининг")
      if (!is.null(input$file_mo_coords)) files_loaded <- c(files_loaded, "Координаты МО")
      if (!is.null(input$file_shapefile)) files_loaded <- c(files_loaded, "Шейпфайл")

      if (length(files_loaded) == 0) {
        div(style = "color: #6c757d;", "Файлы не загружены")
      } else {
        div(style = "color: #00e676;",
            icon("check-circle"),
            paste("Готовы к импорту:", paste(files_loaded, collapse = ", ")))
      }
    })

    # === РЕДАКТИРУЕМЫЕ ТАБЛИЦЫ ===

    # Эпидемиология
    output$table_epi_edit <- renderDT({
      data <- rv$epi
      if (is.null(data) || nrow(data) == 0) {
        return(datatable(data.frame(`Нет данных` = ""), rownames = FALSE))
      }
      # Показываем последние 100 записей для производительности
      data_display <- tail(data, 100)
      datatable(
        data_display,
        editable = list(target = "cell", disable = list(columns = c(0))),
        options  = list(scrollX = TRUE, pageLength = 10),
        rownames = FALSE
      )
    })

    # Обработка редактирования эпидемиологии
    observeEvent(input$table_epi_edit_cell_edit, {
      info <- input$table_epi_edit_cell_edit
      epi <- rv$epi
      # Вычисляем реальный индекс (т.к. показываем tail)
      n <- nrow(epi)
      display_n <- min(n, 100)
      real_row <- n - display_n + info$row
      if (real_row > 0 && real_row <= n) {
        epi[real_row, info$col + 1] <- DT::coerceValue(info$value, epi[real_row, info$col + 1])
        rv$epi <- epi
      }
    })

    # Скрининг
    output$table_scr_edit <- renderDT({
      data <- rv$scr
      if (is.null(data) || nrow(data) == 0) {
        return(datatable(data.frame(`Нет данных` = ""), rownames = FALSE))
      }
      data_display <- tail(data, 100)
      datatable(
        data_display,
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

    # МО
    output$table_mo_edit <- renderDT({
      data <- rv$mo
      if (is.null(data) || nrow(data) == 0) {
        return(datatable(data.frame(`Нет данных` = ""), rownames = FALSE))
      }
      datatable(
        data,
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

    # === СОХРАНЕНИЕ ВСЕХ ИЗМЕНЕНИЙ ===
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

    # === ШАБЛОНЫ ДЛЯ СКАЧИВАНИЯ ===

    # Шаблон эпидемиологии
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
        write_xlsx(tpl, file)
      }
    )

    # Шаблон скрининга
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
        write_xlsx(tpl, file)
      }
    )

    # Шаблон координат
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
        write_xlsx(tpl, file)
      }
    )

  })
}
