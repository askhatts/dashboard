# ============================================================
# ПРОСТРАНСТВЕННЫЕ УТИЛИТЫ
# ============================================================
# Функции для расчёта квартилей, создания цветовых палитр,
# определения размеров символов и конвертации координат.
# ============================================================

# === ПАЛИТРЫ ===
# Палитра хороплета районов: жёлто-синяя (высокий контраст на тёмном фоне)
CHOROPLETH_COLORS <- c("#ffffcc", "#a1dab4", "#41b6c4", "#225ea8")

# Цвет кругов МО: оранжевый (высокий контраст на фоне синих районов)
MO_CIRCLE_COLOR <- "#ff6b35"

# Размеры кругов МО по квартилям (в пикселях радиуса)
MO_CIRCLE_SIZES <- c(6, 10, 15, 22)

# === РАСЧЁТ КВАРТИЛЬНЫХ ГРАНИЦ ===
# Вычисляет 5 граничных значений (0%, 25%, 50%, 75%, 100%)
# для разбиения данных на 4 группы (квартили).
# Обрабатывает крайние случаи:
#   - все значения NA или нулевые
#   - менее 4 уникальных значений
#   - одно уникальное значение
compute_quartile_breaks <- function(values) {
  # Убираем NA
  valid <- values[!is.na(values)]

  # Крайний случай: нет данных
  if (length(valid) == 0) {
    return(c(0, 0.25, 0.5, 0.75, 1))
  }

  # Крайний случай: одно уникальное значение
  if (length(unique(valid)) == 1) {
    v <- valid[1]
    if (v == 0) return(c(0, 0.25, 0.5, 0.75, 1))
    return(c(0, v * 0.5, v, v * 1.5, v * 2))
  }

  # Крайний случай: менее 4 уникальных значений — равные интервалы
  if (length(unique(valid)) < 4) {
    return(seq(min(valid), max(valid), length.out = 5))
  }

  # Стандартный случай: квантили
  breaks <- as.numeric(quantile(valid, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))

  # Если квантили дали дубли (кластеризованные данные) — переходим на равные интервалы
  if (length(unique(breaks)) < 5) {
    breaks <- seq(min(valid), max(valid), length.out = 5)
  }

  breaks
}

# === ПРИСВОЕНИЕ КВАРТИЛЬНОГО КЛАССА ===
# Для каждого значения определяет, в какой квартиль (1-4) оно попадает.
# Значения NA получают класс 1 (минимальный).
assign_quartile_class <- function(values, breaks) {
  # Используем findInterval для определения бина
  # rightmost.closed = TRUE: максимальное значение попадает в последний бин
  classes <- findInterval(values, breaks, rightmost.closed = TRUE)

  # Ограничиваем диапазон 1-4
  classes <- pmin(pmax(classes, 1L), 4L)

  # NA → 1 (минимальный размер/цвет)
  classes[is.na(classes)] <- 1L

  return(classes)
}

# === РАЗМЕРЫ КРУГОВ ПО КВАРТИЛЯМ ===
# Преобразует квартильный класс (1-4) в радиус круга в пикселях.
get_circle_radius <- function(quartile_class) {
  MO_CIRCLE_SIZES[quartile_class]
}

# === КОНВЕРТАЦИЯ КООРДИНАТ В SF-ОБЪЕКТ ===
# Принимает data.frame с колонками широты и долготы,
# возвращает sf-объект с точечной геометрией.
# Используется при загрузке МО из Excel-файла.
coords_to_sf <- function(df, lon_col = "longitude", lat_col = "latitude", crs = 4326) {
  # Фильтруем строки без координат
  valid <- df[!is.na(df[[lon_col]]) & !is.na(df[[lat_col]]), ]

  if (nrow(valid) == 0) {
    warning("Нет строк с валидными координатами")
    return(NULL)
  }

  sf::st_as_sf(valid, coords = c(lon_col, lat_col), crs = crs)
}

# === СОЗДАНИЕ HTML-ЛЕГЕНДЫ РАЗМЕРОВ ===
# Leaflet не имеет встроенной легенды для пропорциональных символов.
# Создаём HTML вручную для отображения через addControl().
create_size_legend_html <- function(indicator_name, breaks) {
  # Формируем 4 элемента легенды с кругами разного размера
  items <- ""
  labels <- c("Низкий", "Ниже среднего", "Выше среднего", "Высокий")

  for (i in 1:4) {
    diameter <- MO_CIRCLE_SIZES[i] * 2
    # Подписи с диапазонами значений
    if (length(breaks) >= 5) {
      range_label <- paste0(round(breaks[i], 1), " – ", round(breaks[i + 1], 1))
    } else {
      range_label <- labels[i]
    }

    items <- paste0(items, sprintf(
      '<div style="display:flex;align-items:center;margin:3px 0;">
        <svg width="%d" height="%d" style="min-width:%dpx;margin-right:8px;">
          <circle cx="%d" cy="%d" r="%d" fill="%s" stroke="white" stroke-width="1" opacity="0.85"/>
        </svg>
        <span style="color:#e0e0e0;font-size:11px;">%s</span>
      </div>',
      max(diameter, 12), max(diameter, 12), max(diameter, 12),
      diameter / 2, diameter / 2, MO_CIRCLE_SIZES[i],
      MO_CIRCLE_COLOR, range_label
    ))
  }

  sprintf(
    '<div style="background:rgba(26,26,46,0.92);padding:10px 12px;border-radius:6px;
                 border:1px solid #30305a;min-width:140px;">
      <div style="color:#e0e0e0;font-weight:600;font-size:12px;margin-bottom:6px;">%s</div>
      %s
    </div>',
    indicator_name, items
  )
}
