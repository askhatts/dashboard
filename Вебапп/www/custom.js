// ============================================================
// JAVASCRIPT — Интерактивность дашборда
// ============================================================
// Обработчики:
//   1. Полноэкранный режим для панелей графиков
//   2. Ресайз plotly при изменении размера контейнера
// ============================================================

// === ПОЛНОЭКРАННЫЙ РЕЖИМ ДЛЯ ПАНЕЛЕЙ ГРАФИКОВ ===
// При клике на кнопку fullscreen — панель разворачивается на весь экран.
// При повторном клике — возвращается в исходное положение.
// Также работает клавиша Escape для выхода.

// Обработчик сообщений от Shiny-сервера
Shiny.addCustomMessageHandler('toggleFullscreen', function(data) {
  var panelId = data.panelId;
  var panel = document.getElementById(panelId);
  if (!panel) return;

  panel.classList.toggle('fullscreen-mode');

  // Принудительный ресайз plotly-графиков внутри панели
  // (иначе графики не перерисуются под новый размер)
  setTimeout(function() {
    var plots = panel.querySelectorAll('.js-plotly-plot');
    plots.forEach(function(plot) {
      if (window.Plotly) {
        Plotly.Plots.resize(plot);
      }
    });
  }, 350);
});

// Выход из полноэкранного режима по нажатию Escape
document.addEventListener('keydown', function(e) {
  if (e.key === 'Escape') {
    var fullscreenPanels = document.querySelectorAll('.floating-panel.fullscreen-mode');
    fullscreenPanels.forEach(function(panel) {
      panel.classList.remove('fullscreen-mode');
      // Ресайз plotly обратно
      setTimeout(function() {
        var plots = panel.querySelectorAll('.js-plotly-plot');
        plots.forEach(function(plot) {
          if (window.Plotly) {
            Plotly.Plots.resize(plot);
          }
        });
      }, 350);
    });
  }
});

// === АВТОМАТИЧЕСКИЙ РЕСАЙЗ PLOTLY ПРИ ИЗМЕНЕНИИ ОКНА ===
// Leaflet и plotly иногда не реагируют на resize при начальной загрузке.
// Этот обработчик принудительно запускает ресайз через 500мс после загрузки.
window.addEventListener('load', function() {
  setTimeout(function() {
    // Ресайз всех plotly-графиков на странице
    var plots = document.querySelectorAll('.js-plotly-plot');
    plots.forEach(function(plot) {
      if (window.Plotly) {
        Plotly.Plots.resize(plot);
      }
    });

    // Ресайз leaflet-карты
    var maps = document.querySelectorAll('.leaflet-container');
    maps.forEach(function(map) {
      if (map._leaflet_id) {
        var leafletMap = L.Map._instances && L.Map._instances[map._leaflet_id];
        if (leafletMap) leafletMap.invalidateSize();
      }
    });
  }, 500);
});
