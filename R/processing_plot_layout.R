# Separate display guides from analytical plots. Main Shiny outputs remain ggplots
# so Shiny can calculate the coordinate maps used by zoom, hover and selection.
plot_without_legend <- function(plot) plot + ggplot2::theme(legend.position = "none")

legend_wrap <- function(labels, width = 32L) {
  if (is.expression(labels) || is.language(labels)) return(labels)
  vapply(as.character(labels), function(label) {
    if (is.na(label)) return(NA_character_)
    lines <- strsplit(label, "\n", fixed = TRUE)[[1L]]
    paste(unlist(lapply(lines, function(line) {
      # strwrap alone does not break long identifiers without spaces.
      words <- strsplit(line, " ", fixed = TRUE)[[1L]]
      words <- unlist(lapply(words, function(word) {
        if (!nzchar(word)) return("")
        starts <- seq.int(1L, nchar(word), by = width)
        substring(word, starts, pmin(starts + width - 1L, nchar(word)))
      }))
      strwrap(paste(words, collapse = " "), width = width)
    })), collapse = "\n")
  }, character(1), USE.NAMES = FALSE)
}

legend_inches <- function(grob) c(
  width = grid::convertWidth(sum(grob$widths), "in", valueOnly = TRUE),
  height = grid::convertHeight(sum(grob$heights), "in", valueOnly = TRUE))

stack_legend_guides <- function(guides) {
  if (!length(guides)) return(NULL)
  widths <- lapply(guides, function(g) sum(g$widths))
  heights <- lapply(guides, function(g) sum(g$heights) + grid::unit(.12, "in"))
  combined <- gtable::gtable(widths = do.call(grid::unit.pmax, widths),
    heights = do.call(grid::unit.c, heights))
  for (i in seq_along(guides)) combined <- gtable::gtable_add_grob(combined,
    grid::grobTree(guides[[i]], vp = grid::viewport(x = 0, y = 1,
      width = sum(guides[[i]]$widths), height = sum(guides[[i]]$heights),
      just = c("left", "top"))), t = i, l = 1, clip = "off")
  combined
}

plot_legend_parts <- function(plot) {
  # Measure on a private device; never open Rplots.pdf or disturb a Shiny device.
  grDevices::pdf(file = NULL, width = 10, height = 8)
  on.exit(grDevices::dev.off(), add = TRUE)
  built <- ggplot2::ggplot_build(plot)
  display <- built$plot
  trained_scales <- built$plot$scales$scales
  aesthetics <- display$guides$aesthetics
  overrides <- list()
  for (aesthetic in aesthetics) {
    guide <- display$guides$get_guide(aesthetic)
    params <- guide$params
    params$position <- "right"
    if (inherits(guide, "GuideLegend") && !inherits(guide, "GuideColourbar")) {
      params$ncol <- 1L; params$nrow <- NULL; params$direction <- "vertical"
    }
    if (is.character(params$title)) params$title <- legend_wrap(params$title)
    overrides[[aesthetic]] <- local({
      parent <- guide; settings <- params
      ggplot2::ggproto(NULL, parent, params = settings)
    })
  }
  if (length(overrides)) display <- display + do.call(ggplot2::guides, overrides)
  # Clone scales before wrapping display labels: original plots and data retain
  # their exact categories, palettes, breaks and labels in every other use.
  display$scales <- display$scales$clone()
  for (i in seq_along(display$scales$scales)) {
    scale <- display$scales$scales[[i]]
    if (!length(intersect(scale$aesthetics, aesthetics))) next
    original <- trained_scales[[i]]
    if (original$is_discrete()) {
      breaks <- original$get_breaks()
      labels <- legend_wrap(original$get_labels(breaks))
      scale$labels <- local({ keys <- as.character(breaks); values <- labels
        function(breaks) values[match(as.character(breaks), keys)] })
    }
    if (is.character(scale$name)) scale$name <- legend_wrap(scale$name)
    display$scales$scales[[i]] <- scale
  }
  for (aesthetic in aesthetics) if (is.character(display$labels[[aesthetic]]))
    display$labels[[aesthetic]] <- legend_wrap(display$labels[[aesthetic]])
  display <- display + ggplot2::theme(legend.position = "right", legend.box = "vertical",
    legend.direction = "vertical", legend.title.position = "top",
    legend.text = ggplot2::element_text(size = 10),
    legend.title = ggplot2::element_text(size = 10),
    legend.margin = ggplot2::margin(5, 5, 5, 5))
  gt <- ggplot2::ggplotGrob(display)
  slots <- which(grepl("^guide-box", gt$layout$name))
  boxes <- gt$grobs[slots]
  guides <- unlist(lapply(boxes, function(box) {
    if (!inherits(box, "gtable")) return(list())
    box$grobs[box$layout$name == "guides"]
  }), recursive = FALSE)
  legend <- stack_legend_guides(guides)
  dimensions <- if (is.null(legend)) c(width = 0, height = 0) else legend_inches(legend)
  list(plot = plot_without_legend(plot), legend = legend, guides = guides,
    width = unname(dimensions["width"]), height = unname(dimensions["height"]))
}

# Split categorical guides between complete key/label rows, repeating their
# titles. Continuous guides remain intact, including the existing percent bar.
paginate_legend_guides <- function(guides, height) {
  pages <- list()
  for (guide in guides) {
    rows <- sort(unique(guide$layout$t[grepl("^key-", guide$layout$name)]))
    if (legend_inches(guide)["height"] <= height || !length(rows)) {
      pages[[length(pages) + 1L]] <- guide
      next
    }
    header <- seq_len(min(rows) - 1L)
    footer <- seq.int(max(rows) + 1L, length(guide$heights))
    slice <- function(indices) {
      body <- unlist(lapply(indices, function(i) seq.int(rows[i],
        if (i < length(rows)) rows[i + 1L] - 1L else rows[i])))
      guide[unique(c(header, body, footer)), ]
    }
    start <- 1L
    while (start <= length(rows)) {
      end <- start
      while (end < length(rows) && legend_inches(slice(seq.int(start, end + 1L)))["height"] <= height)
        end <- end + 1L
      pages[[length(pages) + 1L]] <- slice(seq.int(start, end))
      start <- end + 1L
    }
  }
  pages
}

legend_figure_ui <- function(plot, legend_id, height) {
  shiny::div(class = "metlip-figure", style = paste0("--plot-height:", height, ";"),
    shiny::div(class = "metlip-figure-row",
      shiny::div(class = "metlip-figure-main", plot),
      shiny::uiOutput(legend_id, class = "metlip-legend-slot")))
}

bind_plot_legend <- function(output, session, id, plot) {
  parts <- shiny::reactive(tryCatch(plot_legend_parts(plot()), error = function(e) NULL))
  width <- function() max(280L, ceiling(parts()$width * 110) + 12L)
  height <- function() max(1L, ceiling(parts()$height * 110) + 12L)
  output[[paste0(id, "_legend_ui")]] <- shiny::renderUI({
    p <- parts()
    if (is.null(p) || is.null(p$legend)) return(NULL)
    shiny::div(class = "metlip-legend-panel", tabindex = "0", role = "region", `aria-label` = "Plot legend",
      shiny::plotOutput(session$ns(paste0(id, "_legend")),
        width = paste0(width(), "px"), height = paste0(height(), "px")))
  })
  # CSS hides empty slots; their UI must still run to populate them again.
  shiny::outputOptions(output, paste0(id, "_legend_ui"), suspendWhenHidden = FALSE)
  output[[paste0(id, "_legend")]] <- shiny::renderPlot({
    p <- parts(); shiny::req(!is.null(p$legend))
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(x = 0, y = 1, width = grid::unit(p$width, "in"),
      height = grid::unit(p$height, "in"), just = c("left", "top")))
    grid::grid.draw(p$legend)
    grid::popViewport()
  }, width = width, height = height, res = 110, bg = "white")
  invisible(parts)
}

save_pca_pdf <- function(file, plot, width = 10, height = 8) {
  parts <- plot_legend_parts(plot)
  fits <- !is.null(parts$legend) && parts$height <= height - .5 && parts$width <= 3.2
  extra <- if (fits) parts$width + .3 else 0
  grDevices::pdf(file, width = width + extra, height = height, bg = "white", useDingbats = FALSE)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  print(parts$plot, newpage = FALSE, vp = grid::viewport(x = 0, y = 0,
    width = grid::unit(width, "in"), height = grid::unit(height, "in"), just = c("left", "bottom")))
  if (fits) {
    grid::pushViewport(grid::viewport(x = grid::unit(width + .15, "in"), y = grid::unit(height - .25, "in"),
      width = grid::unit(parts$width, "in"), height = grid::unit(parts$height, "in"), just = c("left", "top")))
    grid::grid.draw(parts$legend); grid::popViewport()
  } else if (!is.null(parts$legend)) {
    pages <- paginate_legend_guides(parts$guides, height - 1)
    for (i in seq_along(pages)) {
      grid::grid.newpage()
      grid::grid.text(paste0("Plot legend - ", i, " / ", length(pages)), x = .05, y = .96,
        just = "left", gp = grid::gpar(fontsize = 12, fontface = "bold"))
      g <- pages[[i]]; size <- legend_inches(g)
      grid::pushViewport(grid::viewport(x = .05, y = .91,
        width = grid::unit(size["width"], "in"), height = grid::unit(size["height"], "in"),
        just = c("left", "top")))
      grid::grid.draw(g); grid::popViewport()
    }
  }
  invisible(file)
}

save_result_pdf <- function(file, plot, width, height, pca = FALSE) {
  if (pca) save_pca_pdf(file, plot, width, height) else
    ggplot2::ggsave(file, plot, device = "pdf", width = width, height = height,
      limitsize = FALSE, useDingbats = FALSE)
}

write_legend_images <- function(parts, directory, stem, dpi = 110) {
  if (is.null(parts$legend)) return(character())
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  pages <- paginate_legend_guides(parts$guides, 7)
  paths <- character()
  for (i in seq_along(pages)) {
    name <- paste0(stem, "_legend_", i, ".png")
    size <- legend_inches(pages[[i]])
    ggplot2::ggsave(file.path(directory, name), pages[[i]], device = "png",
      width = max(2.55, size["width"] + .1), height = size["height"] + .1,
      units = "in", dpi = dpi, bg = "white", limitsize = FALSE)
    paths <- c(paths, name)
  }
  paths
}
