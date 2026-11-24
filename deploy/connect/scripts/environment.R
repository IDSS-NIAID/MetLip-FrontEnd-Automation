# Shared validation; functions only, so this can also be sourced by checks.
check_environment <- function(project, library, github_sha = NULL) {
  lock <- renv::lockfile_read(file.path(project, "renv.lock"))
  if (as.character(getRversion()) != lock$R$Version)
    stop("Use the recorded R version: ", lock$R$Version)
  .libPaths(c(normalizePath(library, mustWork = TRUE), .Library), include.site = FALSE)
  roots <- c("MetLipAutomation", "quarto", "knitr", "rmarkdown", "htmltools", "renv")
  db <- installed.packages()
  required <- unique(c(roots, unlist(tools::package_dependencies(roots, db = db, recursive = TRUE))))
  if (any(!required %in% rownames(db))) stop("The deployment dependency closure is incomplete.")
  required <- required[is.na(db[required, "Priority"]) | db[required, "Priority"] != "base"]
  if (any(!required %in% names(lock$Packages))) stop("The lockfile omits runtime/report dependencies.")
  for (name in names(lock$Packages)) {
    record <- lock$Packages[[name]]
    path <- find.package(name, quiet = TRUE)
    if (!length(path)) stop("Missing locked package: ", name)
    desc <- packageDescription(name)
    if (desc$Version != record$Version) stop("Locked version mismatch: ", name)
    if (identical(record$Source, "GitHub")) {
      for (field in c("RemoteSha", "RemoteUsername", "RemoteRepo"))
        if (!identical(desc[[field]], record[[field]]))
          stop("Locked GitHub provenance mismatch: ", name, " / ", field)
    }
  }
  qc <- lock$Packages$qcdrift
  if (!identical(qc$RemoteSha, "d4cc6399e0ed54748d787290494d0118a0ab9c2c"))
    stop("qcdrift differs from the accepted integration dependency.")
  if (!is.null(github_sha)) {
    if (!grepl("^[0-9a-f]{40}$", github_sha)) stop("Supply the full tested GitHub commit SHA.")
    app <- lock$Packages$MetLipAutomation
    if (!identical(app$Source, "GitHub") || !identical(app$RemoteSha, github_sha) ||
        !identical(app$RemoteUsername, "IDSS-NIAID") ||
        !identical(app$RemoteRepo, "MetLip-FrontEnd-Automation"))
      stop("Candidate is unpublished or does not match the tested GitHub revision.")
    bad <- names(Filter(function(x) !x$Source %in% c("Repository", "GitHub"), lock$Packages))
    if (length(bad)) stop("Unrestorable deployment sources: ", paste(bad, collapse = ", "))
  }
  invisible(lock)
}
