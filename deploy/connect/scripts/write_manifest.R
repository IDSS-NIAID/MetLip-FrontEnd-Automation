# Run only AFTER validating this exact installed GitHub revision.
# Rscript --vanilla scripts/write_manifest.R <candidate-dir> <library-dir> <tested-full-sha>
args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
project <- normalizePath(args[1L], mustWork = TRUE)
source(file.path(project, "scripts/environment.R"))
# Validate provenance before requiring the optional manifest tooling.
check_environment(project, args[2L], github_sha = args[3L])
if (!requireNamespace("rsconnect", quietly = TRUE))
  stop("Install rsconnect in the deployment library before generating the manifest.")
files <- c("app.R", "DESCRIPTION", "renv.lock")
rsconnect::writeManifest(appDir = project, appFiles = files, appPrimaryDoc = "app.R",
  appMode = "shiny", quarto = FALSE, dependencyResolution = "strict")
manifest <- jsonlite::read_json(file.path(project, "manifest.json"), simplifyVector = FALSE)
stopifnot(identical(manifest$packages$MetLipAutomation$description$RemoteSha, args[3L]),
  identical(manifest$packages$qcdrift$description$RemoteSha,
    "d4cc6399e0ed54748d787290494d0118a0ab9c2c"))
cat("Manifest written for tested revision", args[3L], "(no deployment performed).\n")
