# MetLip Front-End Automation

`MetLipAutomation` is an R package and Shiny dashboard for preparing metabolomics/lipidomics sample runs. It supports the workflow from Technology Accessioning System (TAS) sample retrieval through acquired-sample naming, plate loading metadata, and SciexOS-compatible sequence CSV files.

The repository contains:

| Path | Purpose |
|------------------------------------|------------------------------------|
| `R/` | Reusable package functions for metadata, plate, and sequence generation. |
| `RShiny/MetLip_FrontEnd_shiny_dashboard.R` | The current two-tab Shiny dashboard. |
| `RShiny/www/logo.png` | Dashboard branding asset. |
| `tests/testthat/` | Automated tests for plate metadata generation. |
| `scripts/` | Development and exploratory scripts; these are not the supported application entry point. |

## Requirements and installation

The package requires **R 4.4 or later**. Install the package and the dashboard dependencies from an R session:

``` r
pak::pak("IDSS-NIAID/MetLip-FrontEnd-Automation")
```

For local development from a cloned repository, use `devtools::load_all(".")` instead of installing from GitHub.

### Run the dashboard

The dashboard can be loaded as follows (the package must either be installed as described above or you can run `devtools::load_all()` if you want to load the active development version):

``` r
MetLipAutomation::load_MetLip_FrontEnd()
```

The dashboard expects `RShiny/www/logo.png` to remain alongside the script.

### TAS access

The **TAS Accelerator** tab calls the production TAS API at `https://rtb.nih.gov`. Before starting the dashboard with TAS access, set:

| Environment variable | Purpose |
|------------------------------------|------------------------------------|
| `TAS_PCS_API_KEY` | Shared secret used to sign the short-lived TAS identity JWT. |
| `TAS_PCS_UPN` | TAS user principal name placed in the JWT subject claim. |

On Posit Connect, configure these as environment variables in the deployment environment rather than placing them in code or files. The dashboard caches a TAS API token per R process and refreshes it before expiry.

Without valid TAS credentials, the Sequence Generator can still be used by uploading an acquired-samples Excel workbook.

## Dashboard workflow

### 1. TAS Accelerator

This tab creates and confirms acquired samples in TAS.

1.  Enter a **Project Request ID** and select **Search**. The app downloads the submitted-samples workbook from TAS. It recognizes submitted-ID columns named `Submitted Sample Ids`, `SubmittedSampleIds`, `SubmittedSampleId`, or `SampleId`.
2.  Add one or more MS methods. Each submitted sample is repeated once for each method, and its acquired sample name is formed as `<submitted-sample-id>_<MS-method>`.
3.  Select **Generate** to review the expanded acquired-sample table.
4.  Select **Send to TAS** to upload it. The TAS-confirmed workbook becomes the input for the second tab and can also be downloaded as Excel.

The upload includes the acquired sample name, submitted sample ID, and `Notes` for each row. Keep the `Notes` column when it is present.

### 2. Sequence Generator

This tab creates plate-loading files and run sequences.

1.  Choose the data source:
    - **Use acquired samples directly from the TAS Accelerator tab** uses the confirmed TAS workbook from the current session.
    - **Upload Excel file** accepts `.xlsx` or `.xls` acquired-sample workbooks. Supply a project ID when uploading. The app removes a trailing `(blank if new)` from column headers automatically.
2.  Select **Load Data**. Uploaded files must include a supported submitted-sample ID column, `Acquired Sample ID`, `Acquired Sample Name`, and `Notes`.
3.  Assign sample matrix/matrices:
    - For one matrix, enter its name.
    - For multiple matrices, enter each matrix name, choose **Select Sample Range**, then click the first and last sample in the reference table and add the range. Ranges follow the displayed submitted-sample order.
4.  Select **Validate & Preview**. The app adds `Project_ID`, `Acquired_Sample_ID`, and processing date fields. It derives `MS_method` from the text following the final underscore in each acquired sample name.
5.  Choose randomization behavior:
    - **No Randomization** preserves plate-loading order.
    - Otherwise, optionally select uploaded extra columns as grouping variables. The app randomizes group order and randomizes samples within each group. You can drag level tiles into a fixed order and select **Save Order**; unspecified variables and levels retain randomized ordering.
6.  Select **Generate Plate Meta Data**. Review each matrix's plate loading table, provide LC and full SciexOS MS method names, and configure QC/blank plate and vial locations, injection volume, and the QC/blank interval.
7.  Select **Generate Sequence**, then **Download**. The ZIP contains:
    - `plate_data/`: one CSV and one Excel plate-loading file per matrix.
    - `sequences/`: CSV sequence files, one per MS method, matrix, and batch.

Sequence files begin with four blanks, then one QC and one blank; insert a QC and blank between sample chunks at the selected interval; and end with one QC and one blank. Default QC and blank locations are plate 3, vials 53 and 54, respectively; the dashboard exposes them as editable inputs.

## Package functions

Load the package with:

``` r
library(MetLipAutomation)
```

| Function | Description |
|------------------------------------|------------------------------------|
| `generate_sample_meta_data()` | Expands project, subject, condition, matrix, and replicate inputs into submitted sample metadata; optionally writes CSV. |
| `generate_plate_meta_data()` | Assigns submitted samples to batches, plates, and 54-vial positions, reserving QC and blank locations; optionally writes CSV. |
| `process_plate_data()` | Joins plate metadata and acquired-sample data and returns plate loading, QC, blank, and project-ID data. |
| `generate_sequence()` | Writes SciexOS-formatted CSV sequences with randomization and QC/blank insertion controls. |
| `process_acquisition_ids()` | Expands submitted samples by selected built-in MS methods and creates acquired IDs; it expects an Excel workbook in the working directory and writes an acquired-samples workbook. |
| `process_acquisition_ids_reduced()` | Adds a project ID parsed from the first Excel filename in the working directory. |
| `append_project_name()` | Adds or replaces a project-ID column in a data frame. |
| `vectorize_input()` | Converts delimited interactive console input to a vector. |

### Typical programmatic pipeline

``` r
sample_meta <- generate_sample_meta_data(
  project_id = "example-project",
  conditions = c("control", "treated"),
  matrices = c("plasma"),
  replicates = 1,
  num_subjects = 12,
  path = NULL
)

plate_meta <- generate_plate_meta_data(sample_meta, path = NULL)
# Acquired data must contain Project_ID, Submitted Sample Ids, Matrix,
# MS_method, Acquired_Sample_ID, and the fields used by process_plate_data().
plate_results <- process_plate_data(plate_meta, acquired_samples)

generate_sequence(
  plate_results$plate_loading,
  plate_results$qc_plate_data,
  plate_results$blank_plate_data,
  plate_results$project_id,
  injection_vol = 5,
  output_dir = "sequence-output"
)
```

## Data and output conventions

- `generate_plate_meta_data()` uses 54 positions per plate, one blank per batch, one QC per matrix, and three plates per batch unless its parameters are changed.
- Plate assignment deduplicates by `Matrix` and `Submitted Sample Ids`; the same submitted ID can therefore be represented once per matrix.
- Randomized sequence generation is reproducible: it uses deterministic, independent seeds for each MS method, matrix, and batch combination.
- Sequence CSVs use the SciexOS columns `Sample Name`, `MS Method`, `LC Method`, `Rack Type`, `Plate Type`, `Plate Position`, `Vial Position`, `Injection Volume`, `Sample Type`, and `Data File`.
- `process_acquisition_ids()` and `process_acquisition_ids_reduced()` infer project details from the first `.xlsx` filename in the current working directory. Run them in a directory containing only the intended input workbook.

## Development

Run the package tests from the repository root:

``` r
devtools::test()
```

Generate or refresh package documentation after changing roxygen comments:

``` r
devtools::document()
```

The project is distributed under the [MIT License](LICENSE.md).