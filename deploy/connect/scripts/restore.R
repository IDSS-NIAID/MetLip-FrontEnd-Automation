# Rscript --vanilla scripts/restore.R <candidate-dir> [library-dir]
args <- commandArgs(TRUE)
stopifnot(length(args) %in% c(1L, 2L))
project <- normalizePath(args[1L], mustWork = TRUE)
library <- if (length(args) == 2L) args[2L] else file.path(project, "library")
dir.create(library, recursive = TRUE, showWarnings = FALSE)
library <- normalizePath(library, mustWork = TRUE)
lock <- renv::lockfile_read(file.path(project, "renv.lock"))
if (as.character(getRversion()) != lock$R$Version) stop("Use R ", lock$R$Version)
local_candidate <- identical(lock$Packages$MetLipAutomation$Source, "Local")
if (local_candidate) {
  archive <- file.path(project, lock$Packages$MetLipAutomation$RemoteUrl)
  release <- jsonlite::read_json(file.path(project, "release.json"), simplifyVector = TRUE)
  if (!file.exists(archive) || !identical(digest::digest(archive, algo = "sha256", file = TRUE),
      release$tarball_sha256)) stop("Local candidate archive is missing or changed; rebuild the candidate.")
}
# Resolve relative local records against the candidate, including when invoked
# from an unrelated directory. A local archive is installed by R directly;
# renv handles remotely restorable dependencies and the future GitHub package.
setwd(project)
renv::restore(project = project, library = library,
  exclude = if (local_candidate) "MetLipAutomation" else NULL, prompt = FALSE)
if (local_candidate) utils::install.packages(archive, repos = NULL, type = "source", lib = library)
source(file.path(project, "scripts/environment.R"))
check_environment(project, library)
cat("Locked deployment environment restored to", library, "\n")
