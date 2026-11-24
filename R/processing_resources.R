# Installed resources are resolved when used, never while building/loading the
# namespace. Export writers receive a caller-owned temporary directory.
metlip_processing_resource <- function(..., must_work = TRUE) {
  system.file("processing", ..., package = "MetLipAutomation", mustWork = must_work)
}

metlip_report_template <- function() {
  # Missing report resources must not prevent numerical processing or CSV/PCA
  # exports. write_report_source() validates this path when a report is requested.
  metlip_processing_resource("reports", "lipidomics_report.qmd", must_work = FALSE)
}

metlip_processing_dependency <- function() {
  htmltools::htmlDependency(
    name = "metlip-processing", version = "2.4.7",
    src = c(file = metlip_processing_resource("www")),
    script = "plot_zoom.js", stylesheet = "plot_layout.css",
    all_files = FALSE
  )
}

metlip_processing_module_dependency <- function() {
  htmltools::htmlDependency(
    name = "metlip-processing-module", version = "2.4.7",
    src = c(file = metlip_processing_resource("www")),
    script = "processing_module.js", stylesheet = "processing_module.css",
    all_files = FALSE
  )
}

metlip_branding_dependency <- function() {
  htmltools::htmlDependency(
    name = "metlip-branding", version = "1.0.0",
    src = c(file = system.file("branding", package = "MetLipAutomation", mustWork = TRUE)),
    attachment = "logo.png", all_files = FALSE
  )
}

# Shared by the nested PCA/comparison modules. DT versions before fill support
# still render tables in normal document flow.
table_output <- function(output_id) {
  args <- list(outputId = output_id, width = "100%", height = "auto")
  if ("fill" %in% names(formals(DT::DTOutput))) args$fill <- FALSE
  do.call(DT::DTOutput, args)
}
