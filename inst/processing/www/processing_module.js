/* One dependency and one handler set, even with multiple processing instances. */
(function () {
  "use strict";
  function rootFor(target) {
    return target && target.closest ? target.closest(".metlip-processing") : null;
  }
  function ownedElement(root, id) {
    var element = document.getElementById(id);
    return element && root.contains(element) ? element : null;
  }
  function activeTab(root) {
    var tabs = ownedElement(root, root.getAttribute("data-metlip-prefix") + "tabs");
    return tabs && tabs.querySelector("li.active > a");
  }
  var resizePending = false;
  function resizePlots() {
    if (resizePending) return;
    resizePending = true;
    // Wait for the tab/layout classes to apply before Shiny measures outputs.
    window.requestAnimationFrame(function () {
      resizePending = false;
      window.dispatchEvent(new Event("resize"));
    });
  }
  function updateLayout(root) {
    var tab = activeTab(root);
    var wide = ownedElement(root, root.getAttribute("data-metlip-prefix") + "pca-wide");
    var expanded = !!(tab && tab.getAttribute("data-value") === "pca" && wide && wide.checked);
    if (root.classList.contains("pca-wide-layout") === expanded) return;
    root.classList.toggle("pca-wide-layout", expanded);
    resizePlots();
  }
  function scrollTab(root, tab) {
    if (tab !== "plots" && tab !== "compare") return;
    window.requestAnimationFrame(function () {
      var active = activeTab(root);
      if (active && active.getAttribute("data-value") === tab) root.scrollIntoView({block: "start"});
    });
  }
  function messageRoot(message) {
    var root = document.getElementById(message.container);
    return root && root.matches(".metlip-processing") ? root : null;
  }
  Shiny.addCustomMessageHandler("metlip-processing-scroll", function (message) {
    var root = messageRoot(message);
    if (root) scrollTab(root, message.tab);
  });
  Shiny.addCustomMessageHandler("metlip-processing-pathways", function (message) {
    var root = messageRoot(message);
    if (!root) return;
    window.requestAnimationFrame(function () {
      var panel = ownedElement(root, message.panel);
      if (panel) { panel.open = true; panel.scrollIntoView({block: "start"}); }
    });
  });
  document.addEventListener("change", function (event) {
    var target = event.target, root = rootFor(target);
    if (!root) return;
    var prefix = root.getAttribute("data-metlip-prefix");
    if (target.id === prefix + "pca-wide") updateLayout(root);
    if (target.matches("input[type=checkbox]") && target.closest(".duplicate-highest-control")) {
      Shiny.setInputValue(prefix + "duplicate_highest_event", {
        editor_id: target.id, checked: target.checked
      }, {priority: "event"});
    } else if (target.matches("select") && target.closest(".duplicate-row-decision")) {
      Shiny.setInputValue(prefix + "duplicate_decision_event", {
        editor_id: target.id, decision: target.value
      }, {priority: "event"});
    }
  }, true);
  $(document).on("shown.bs.tab.metlipProcessing", function (event) {
    var root = rootFor(event.target);
    if (root) {
      updateLayout(root);
      resizePlots();
      if (event.target === activeTab(root)) scrollTab(root, event.target.getAttribute("data-value"));
      return;
    }
    // The dashboard menu link lives outside the module. Resolve its local pane
    // by ID rather than hard-coding a module/tab name or using a global selector.
    var href = event.target && event.target.getAttribute("href");
    var pane = href && href.charAt(0) === "#" && document.getElementById(href.slice(1));
    if (!pane) return;
    var roots = pane.querySelectorAll(".metlip-processing");
    roots.forEach(updateLayout);
    if (roots.length) resizePlots();
  });
  var observed = new WeakSet();
  var widths = new WeakMap();
  // AdminLTE's sidebar animates without a window resize. Also watch layout
  // width so nested plots/legends are remeasured at its final available size.
  var observer = typeof ResizeObserver === "function" ? new ResizeObserver(function (entries) {
    var changed = false;
    entries.forEach(function (entry) {
      var width = entry.contentRect.width;
      if (width > 0 && widths.get(entry.target) !== width) changed = true;
      widths.set(entry.target, width);
    });
    if (changed) resizePlots();
  }) : null;
  function initialize() {
    document.querySelectorAll(".metlip-processing").forEach(function (root) {
      updateLayout(root);
      if (observer && !observed.has(root)) {
        observer.observe(root);
        observed.add(root);
      }
    });
  }
  $(document).on("shiny:connected.metlipProcessing shiny:bound.metlipProcessing", initialize);
  $(initialize);
})();
