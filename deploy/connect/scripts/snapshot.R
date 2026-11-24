# Rscript --vanilla snapshot.R <candidate-dir> <installed-library> <local|github> [full-sha]
args <- commandArgs(TRUE)
stopifnot(length(args) %in% c(3L, 4L), args[3L] %in% c("local", "github"))
project <- normalizePath(args[1L], mustWork = TRUE)
library <- normalizePath(args[2L], mustWork = TRUE)
.libPaths(c(library, .libPaths()))
options(repos = c(CRAN = "https://cloud.r-project.org"))
roots <- trimws(unlist(strsplit(read.dcf(file.path(project, "DESCRIPTION"))[1L, "Imports"], ",")))
desc <- packageDescription("MetLipAutomation", lib.loc = library)
stopifnot(!is.na(desc$Package), desc$Package == "MetLipAutomation")
if (args[3L] == "github") {
  stopifnot(length(args) == 4L, grepl("^[0-9a-f]{40}$", args[4L]),
    identical(desc$RemoteSha, args[4L]), identical(desc$RemoteUsername, "IDSS-NIAID"),
    identical(desc$RemoteRepo, "MetLip-FrontEnd-Automation"))
}
# Source inference is unavailable for the unpublished package. Snapshot the
# dependency closure first, then supply its honest local record explicitly.
dep_roots <- trimws(sub("[(].*", "", unlist(strsplit(desc$Imports, ","))))
lock <- renv::snapshot(project = project, library = .libPaths(), lockfile = NULL,
  packages = unique(c(setdiff(roots, "MetLipAutomation"), dep_roots)), prompt = FALSE)
record <- if (args[3L] == "github") renv::snapshot(description = desc) else list(
  Package = "MetLipAutomation", Version = desc$Version, Source = "Local",
  RemoteType = "local", RemoteUrl = paste0("renv/cellar/MetLipAutomation_", desc$Version, ".tar.gz"))
record$Requirements <- sort(unique(c("R", dep_roots)))
lock$Packages$MetLipAutomation <- record
# Preserve installed RemoteRef metadata; RemoteSha is the immutable restore pin.
fields <- c("Package", "Version", "Source", "Repository", "RemoteType", "RemoteHost",
  "RemoteUsername", "RemoteRepo", "RemoteRef", "RemoteSha", "RemoteUrl", "Hash", "Requirements")
lock$Packages <- lapply(lock$Packages, function(x) x[intersect(fields, names(x))])
lock$Packages <- lock$Packages[sort(names(lock$Packages))]
renv::lockfile_write(lock, file.path(project, "renv.lock"))
versions <- do.call(rbind, lapply(lock$Packages, function(p) data.frame(
  Package = p$Package, Version = p$Version, Source = p$Source,
  Revision = if (is.null(p$RemoteSha)) "" else p$RemoteSha)))
utils::write.csv(versions, file.path(project, "DEPENDENCIES.csv"), row.names = FALSE)
cat("Recorded", nrow(versions), "packages; mode:", args[3L], "\n")
