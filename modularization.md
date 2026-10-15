# Shiny Dashboard Modularization Plan

## Goal

Refactor `RShiny/MetLip_FrontEnd_shiny_dashboard.R` into a small application entry point that sets up shared configuration and the dashboard shell, then calls one Shiny module for each tab. Preserve the current two-tab workflow, styling, TAS integration, and app-object return behavior.

## Proposed file layout

| File | Responsibility |
|---|---|
| `RShiny/MetLip_FrontEnd_shiny_dashboard.R` | Load required packages and module/helper scripts; initialize TAS configuration and the shared token cache; connect the modules in `server`; pass their UI results to the shell builder; return `shinyApp(ui, server)`. |
| `RShiny/dashboard_shell.R` | Define `dashboard_shell(tas_ui, sequence_ui)` to provide the shared dashboard header, sidebar navigation, CSS, Shiny JavaScript setup, and tab containers for the two module UIs. |
| `RShiny/modules/tas_accelerator_module.R` | Define `tas_accelerator_ui(id)` and `tas_accelerator_server(id, tas_api)`. Own the TAS search, MS-method selection, acquired-sample generation, TAS upload, and confirmed-workbook download workflows. |
| `RShiny/modules/sequence_generator_module.R` | Define `sequence_generator_ui(id)` and `sequence_generator_server(id, tas_samples, detect_sub_id_col)`. Own data loading, matrix assignment, validation, randomization/group-level ordering, plate generation, sequence generation, and ZIP download. |
| `R/tas_client.R` | Define `new_tas_client()` to configure TAS URLs and credentials, cache API tokens, sign identity JWTs, and expose download/upload/submitted-ID-detection operations. |

## Module interfaces and shared state

- Use `shiny::NS(id)` in each module UI and `shiny::moduleServer(id, function(input, output, session) { ... })` in each module server. All input/output IDs and JavaScript-generated Shiny input IDs must be module-scoped. Any `shinyjs::show()`, `hide()`, `enable()`, or `disable()` calls must use namespaced IDs (for example, `session$ns("upload_section")`).
- The `dashboard_shell()` function in `RShiny/dashboard_shell.R` owns the existing `dashboardPage`, header, sidebar, shared CSS, and `useShinyjs()`. The main script calls each module UI with a stable ID such as `"tas"` or `"sequence"` and passes the resulting UI to `dashboard_shell()`.
- In the main server, create the TAS API interface once and call each module server once. Have `tas_accelerator_server()` return a reactive value containing the confirmed acquired-sample data and request/project ID. Pass that reactive value to `sequence_generator_server()` so the sequence tab can use TAS-confirmed data without reaching into another module's input/output state.
- Keep tab-specific `reactiveValues`, `reactiveVal`s, observers, renderers, and download handlers inside their respective module servers. Do not share UI IDs, server state, or module globals between tabs.

## Function placement

### Main application entry point

Keep only application composition and process-level setup here:

- Source `R/tas_client.R` and initialize the configured TAS client with `new_tas_client()`.
- Load the module definitions.
- Call the two module UI functions with stable IDs and pass their results to `dashboard_shell()`; call each module server once.
- Return `shinyApp(ui, server)`.

### TAS Accelerator module

Move the current first-tab UI and corresponding server observers/renderers, including:

- Search status and submitted-workbook loading.
- MS method add/remove/clear controls and generated acquired-sample table.
- Upload request construction, upload status, and confirmed workbook parsing.
- Confirmed-workbook Excel download.

Return confirmed data and its project/request ID as a module-owned reactive value for the sequence module.

### Sequence Generator module

Move the current second-tab UI and corresponding server observers/renderers, including:

- TAS-vs-upload source selection, uploaded workbook parsing, and project ID handling.
- Reference table, sample-range selection, matrix assignment, and validation preview.
- Randomization grouping and draggable level-order controls.
- Plate metadata processing, parsed-column joins, method inputs, and plate table.
- Sequence generation and ZIP assembly/download.

## Migration steps

1. Keep TAS client configuration, token caching, and HTTP operations in `R/tas_client.R`; initialize the client in the dashboard entry point.
2. Extract the TAS Accelerator UI and server into `tas_accelerator_module.R`, namespace all IDs (including dynamically generated IDs), and return the confirmed-data reactive interface.
3. Extract the Sequence Generator UI and server into `sequence_generator_module.R`; replace its direct reads of TAS tab state with the passed reactive interface.
4. Reduce the dashboard script to package/helper setup, TAS client initialization, module composition, and `shinyApp()`; put the shared dashboard frame in `dashboard_shell.R`.
5. Run package tests and launch the dashboard. Smoke-test TAS search/generate/upload/download, direct TAS-to-sequence data transfer, Excel upload, matrix assignment, validation, grouping/order controls, plate and sequence generation, and ZIP download.

## Acceptance criteria

- The dashboard entry point contains setup and app composition rather than tab-specific UI/server workflows.
- Each visible tab has exactly one `moduleServer()` module and a matching namespaced UI function.
- The sequence module consumes confirmed TAS data only through the explicit reactive module interface; uploaded-file behavior remains available.
- The existing workflow, outputs, status/error notifications, branding, and `shinyApp` return contract remain intact.
