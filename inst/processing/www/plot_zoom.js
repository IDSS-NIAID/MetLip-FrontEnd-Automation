/* Complete Shiny brushes on mouse release, never on a mid-drag debounce. */
(function () {
  "use strict";
  var active = null;
  var generations = Object.create(null);

  function cancel() { active = null; }
  function cancelWithin(event) {
    if (!active) return;
    var root = active.plot.closest(".metlip-processing");
    var href = event.target && event.target.getAttribute && event.target.getAttribute("href");
    var pane = href && href.charAt(0) === "#" && document.getElementById(href.slice(1));
    // Controls and tab navigation only cancel the owning module's drag.
    if (!root || !event.target || root.contains(event.target) || (pane && pane.contains(active.plot))) cancel();
  }
  function plotAt(target) {
    var plot = target && target.closest ? target.closest("[data-zoom-complete]") : null;
    return plot && plot.closest(".metlip-processing") ? plot : null;
  }

  // Capture before Shiny starts brushing. Only marked PCA/comparison plots opt in.
  document.addEventListener("mousedown", function (event) {
    if (event.button !== 0) return;
    cancel();
    var plot = plotAt(event.target);
    if (!plot) return;
    var id = plot.getAttribute("data-zoom-complete");
    active = {
      plot: plot, id: id, brushId: plot.getAttribute("data-brush-id"),
      generation: generations[id] || 0, brush: null
    };
  }, true);

  // Keep Shiny's data-unit coordinates, log-axis information and facet metadata.
  // The R modules deliberately ignore these intermediate brush inputs.
  $(document).on("shiny:inputchanged.metlipZoom", function (event) {
    if (!active || event.name !== active.brushId) return;
    if (!event.value) { cancel(); return; }
    active.brush = event.value;
  });

  // Shiny flushes its final brush on document mouseup. Window bubbling runs after
  // that flush, including releases outside the plot and a pause before release.
  window.addEventListener("mouseup", function (event) {
    if (event.button !== 0 || !active) return;
    var completed = active;
    cancel();
    var brush = completed.brush;
    var css = brush && brush.coords_css;
    if (!completed.plot.isConnected || !css ||
        !Number.isFinite(css.xmin) || !Number.isFinite(css.xmax) ||
        !Number.isFinite(css.ymin) || !Number.isFinite(css.ymax) ||
        css.xmax - css.xmin < 3 || css.ymax - css.ymin < 3) return;
    Shiny.setInputValue(completed.id, {
      brush: brush, generation: completed.generation
    }, {priority: "event"});
  });

  window.addEventListener("blur", cancel);
  document.addEventListener("change", cancelWithin, true);
  document.addEventListener("dblclick", function (event) {
    if (plotAt(event.target)) cancelWithin(event);
  }, true);
  $(document).on("shown.bs.tab.metlipZoom", cancelWithin);
  $(document).on("hidden.bs.tab.metlipZoom", cancelWithin);
  $(document).on("shiny:disconnected.metlipZoom", cancel);
  // Cancel an in-flight drag when Shiny replaces its coordinate map.
  $(document).on("shiny:value.metlipZoom", function (event) {
    if (active && event.name === active.plot.id) cancel();
  });
  Shiny.addCustomMessageHandler("plot-zoom-cancel", function (message) {
    generations[message.id] = message.generation;
    if (active && active.id === message.id) cancel();
  });
})();
