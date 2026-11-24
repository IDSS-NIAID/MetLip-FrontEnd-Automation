# Connect candidate preparation

This folder is a deployment scaffold. The integration is unpublished: there is
no verified GitHub SHA for `MetLipAutomation` yet. The checked-in `renv.lock`
records the tested local dependency versions and an honest **Local** application
record. It becomes restorable when the Phase 6 runner supplies its matching
source archive under `renv/cellar/`. It is not a Connect manifest.

The scaffold is excluded from the R package build. The installed package holds
the UI, modules, browser resources and report templates; `app.R` only sets the
100 MiB request limit and calls `MetLipAutomation::load_MetLip_FrontEnd()`.

## Prepare and launch locally

Run from the integration workspace with R 4.6.1 and the existing dependencies:

```bash
python3 integration/phase6/run_checks.py \
  --output integration/phase6/evidence/local-rerun \
  --candidate integration/phase6/candidates/local-rerun
```

Use fresh output paths. Exit 0 means the Phase 6 local checks pass; exit 1 is a
check/build failure; exit 2 records a denied loopback launch. The runner copies
installed dependencies into a temporary isolated library, builds the package,
generates a lockfile, removes the installed app, synchronizes dependencies with
`renv`, and reinstalls the retained archive with R's source installer. It checks namespace/numerical parity, launcher
startup, all three tabs, the upload option, mocked TAS/Sequence workflows and a
real HTTP launch. No TAS credentials or live requests are used. Test libraries
and caches are removed; the source archive and candidate files remain reviewable.

To keep a library for interactive use, restore the candidate (network access may
be required for dependencies). Bootstrap `renv` 1.2.4, `jsonlite` and `digest` in
your R environment first if they are absent. From the workspace:

```bash
Rscript --vanilla integration/phase6/candidates/local-rerun/scripts/restore.R \
  integration/phase6/candidates/local-rerun
```

Then, in R from the workspace:

```r
candidate <- normalizePath("integration/phase6/candidates/local-rerun")
.libPaths(c(file.path(candidate, "library"), .Library), include.site = FALSE)
shiny::runApp(candidate, launch.browser = TRUE)
```

`restore.R` checks the local archive's SHA-256 against `release.json`, restores
locked dependencies, installs the archive, and verifies versions and GitHub provenance. The local
runner verifies restoration from the supplied archive and preinstalled
dependencies. Fresh CRAN/GitHub retrieval and Linux restoration are separate
checks, still required before hosting.

## Dependency record

`DEPENDENCIES.csv` lists the complete locked runtime closure. Key accepted
versions are R 4.6.1, Shiny 1.14.0, ggplot2 4.0.3, qcdrift 0.1.0, quarto R 1.5.1,
knitr 1.51, rmarkdown 2.32, htmltools 0.5.9 and renv 1.2.4. The exact qcdrift
revision is `d4cc6399e0ed54748d787290494d0118a0ab9c2c` in both the package's
`Remotes` and the lockfile's `RemoteSha`. The installed package's original
`RemoteRef: HEAD` metadata is preserved; the full `RemoteSha` pins restoration.
Do not replace that SHA with a moving branch or HEAD.

Report packages appear in the environment's `Imports`, so optional package
suggestions cannot disappear during deployment dependency discovery. `testthat`
and `xml2` support local validation; they are outside the runtime lock unless
required transitively. `rsconnect` is manifest tooling, not an app dependency;
record its installed version with the manifest evidence.

## Pin a GitHub candidate after publication

Publication is a separate authorized step. Once the integrated package is
available, record its full 40-character SHA and validate an installation from
that revision. Do not claim the current local archive was fetched from GitHub.

Copy this scaffold to a fresh release folder. In R, set real paths and the
actual published SHA, install `renv`, `remotes` and `rsconnect` in `library`, then
restore the locked dependencies and install only that package revision:

```r
candidate <- normalizePath("/path/to/fresh-release-folder")
library <- file.path(candidate, "library")
dir.create(library, showWarnings = FALSE)
.libPaths(c(library, .libPaths()))
sha <- "REPLACE_WITH_THE_TESTED_40_CHARACTER_GITHUB_SHA"
stopifnot(grepl("^[0-9a-f]{40}$", sha))
renv::restore(project = candidate, library = library,
              exclude = "MetLipAutomation", prompt = FALSE)
remotes::install_github(paste0("IDSS-NIAID/MetLip-FrontEnd-Automation@", sha),
                       lib = library, dependencies = FALSE, upgrade = "never")
```

Write a new GitHub lock from the actual installed metadata (never edit a local
record to invent provenance):

```bash
Rscript --vanilla /path/to/fresh-release-folder/scripts/snapshot.R \
  /path/to/fresh-release-folder /path/to/fresh-release-folder/library github FULL_SHA
```

Validate that installed GitHub package using the retained Phase 2 numerical,
Phase 4 workflow and Phase 5 report/session checks, as well as `app.R` startup
and real HTTP/browser acceptance. Record exact commands, logs, revision,
lockfile hash, R/tool versions and report results together. The Phase 6 local
runner always builds this workspace; rerunning it alone does **not** validate
a separately installed GitHub revision.

For example, from an unrelated directory, set these shell variables to real
absolute paths and run against that GitHub installation:

```bash
workspace=/absolute/path/to/MetLip-FrontEnd-Automation_Galenspace
release_library=/absolute/path/to/fresh-release-folder/library
release_evidence=/absolute/path/to/fresh-release-evidence
mkdir -p "$release_evidence"
Rscript --vanilla "$workspace/integration/phase2/check_installed.R" \
  "$workspace" "$release_library" "$release_evidence"
Rscript --vanilla "$workspace/integration/phase4/check_dashboard.R" \
  "$workspace" "$release_library"
Rscript --vanilla "$workspace/integration/phase5/check_downloads.R" "$workspace" "$release_library"
Rscript --vanilla "$workspace/integration/phase5/check_sessions.R" "$workspace" "$release_library"
Rscript --vanilla "$workspace/integration/phase5/check_failures.R" "$workspace" "$release_library"
Rscript --vanilla "$workspace/integration/phase5/check_reports.R" \
  "$workspace" "$release_library" test_report_qc_availability "$release_evidence"
Rscript --vanilla "$workspace/integration/phase5/check_reports.R" \
  "$workspace" "$release_library" test_report_presentation "$release_evidence"
python3 "$workspace/integration/phase5/check_concurrent.py" \
  "$workspace" "$release_library" "$release_evidence"
python3 "$workspace/integration/phase6/probe_http.py" \
  /absolute/path/to/fresh-release-folder "$release_library" "$release_evidence"
```

Install the validation tools (`testthat`, `xml2`, `codetools`) into that library
when needed. Review the Phase 4/5 browser checklists with that same installation.
Do not generate the manifest if those checks fail. The `check_candidate.R`
unpublished-source rejection assertions are specific to the local candidate;
use the public entry point/HTTP probe for the future GitHub launch.

## Write the manifest without deploying

After the exact GitHub revision passes validation:

```bash
Rscript --vanilla /path/to/fresh-release-folder/scripts/write_manifest.R \
  /path/to/fresh-release-folder /path/to/fresh-release-folder/library FULL_SHA
```

The helper rejects local/unknown sources, a mismatched SHA, changed dependency
versions, and changed qcdrift provenance. It calls `rsconnect::writeManifest()`
with strict lockfile resolution and an explicit file list: `app.R`,
`DESCRIPTION`, `renv.lock`. Libraries, credentials, source archives, fixtures,
evidence and helper scripts are excluded. This is a Shiny app; `quarto = FALSE`
controls bundle type, while reports call Quarto separately at runtime. The
helper checks both GitHub SHAs in the resulting manifest and does not deploy.

Regenerate and review the manifest after changing bundled files or dependencies.
Retain it and its tool versions alongside the tested revision. Current local
validation cannot execute its successful path: `rsconnect` is missing and no
published integration SHA exists. The rejection of an unpublished candidate is
tested. These instructions follow Posit's [manifest API](https://rstudio.github.io/rsconnect/reference/writeManifest.html)
and [strict dependency resolution](https://rstudio.github.io/rsconnect/reference/appDependencies.html).

## Remaining host requirements

- Provide the recorded R runtime and Linux system libraries/build tools needed
  by the locked packages. Validate restoration on the actual Connect host;
  local macOS installations do not establish Linux compatibility.
- Install **Quarto CLI separately**; the accepted local reports used 1.9.38.
  The R `quarto` package does not install the executable. Set `QUARTO_PATH` to
  the full executable path for the application's process, or provide it on
  that process's `PATH`. Connect's configured Quarto discovery and an app's
  runtime environment should both be checked. See [Connect's Quarto setup](https://docs.posit.co/connect/admin/quarto/).
- Allow the application identity to write its R temporary directory and
  Quarto/Sass cache. Confirm a fully styled render; `--version` alone is
  insufficient. No home/cache overrides are shipped in `app.R`.
- Permit dependency retrieval from configured CRAN/GitHub sources, including
  any organization-required private-repository authentication. Store secrets
  in the hosting environment, never in the lockfile or bundle.
- Preserve `TAS_PCS_API_KEY`, `TAS_PCS_UPN`, production TAS configuration and
  existing Connect access controls. The release scaffold does not change them.
- Confirm upstream proxy/request limits permit the application's 100 MiB
  setting. Size workers and timeouts from representative processing/report
  runs; Phase 6 does not establish memory/CPU capacity or concurrency limits.

Phase 7 requires an explicit deployment request and a separate staging app.
Validate restoration, uploads, processing, reports, concurrent sessions,
temporary cleanup and resource usage there before production promotion. Keep
the previous production bundle for rollback.
