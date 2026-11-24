#' Total Sum Normalization (TSN) for Long-Format -Omics Data
#'
#' Performs total-sum normalization on long-format -omics data, where each row
#' represents a single measurement. Sample totals are scaled to a common
#' averaged total sum.
#'
#' The normalization proceeds in two stages:
#' \enumerate{
#'   \item Each value is divided by the total signal of its sample.
#'   \item All normalized values are multiplied by the global mean of the
#'         sample totals.
#' }
#'
#' @param df A data.frame in long format with at minimum: a sample identifier
#'   column, a compound identifier column (e.g., metabolite name), and a value
#'   column containing intensities.
#' @param sample_col Character or integer specifying the sample identifier column.
#' @param compound_col Character or integer specifying the compound identifier column
#'   (e.g., metabolite, lipid, etc.).
#' @param value_col Character or integer specifying the column containing
#'   peak area values to normalize.
#' @param meta_cols Character vector, integer vector, or NULL specifying which
#'   columns contain metadata to preserve. If NULL (default), all columns except
#'   sample_col, compound_col, and value_col are treated as metadata.
#'
#' @return A data.frame in long format with the same structure as the input,
#'   but with normalized values in the value column.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' norm_df <- tsn_long(df, 
#'                     sample_col = "SampleID", 
#'                     compound_col = "Metabolite",
#'                     value_col = "Value")
#'
#' # With explicit metadata columns
#' norm_df <- tsn_long(df,
#'                     sample_col = "SampleID",
#'                     compound_col = "Metabolite", 
#'                     value_col = "Value",
#'                     meta_cols = c("Group", "Timepoint", "Condition"))
#' }
#'
#' @export
tsn_long <- function(df, sample_col, compound_col, value_col, meta_cols = NULL) {
  
  # Convert column specifications to names if they're indices
  sample_name <- if (is.numeric(sample_col)) names(df)[sample_col] else sample_col
  feature_name <- if (is.numeric(compound_col)) names(df)[compound_col] else compound_col
  value_name <- if (is.numeric(value_col)) names(df)[value_col] else value_col
  
  # Validate required columns exist
  required <- c(sample_name, feature_name, value_name)
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("Required columns not found in data: ", paste(missing, collapse = ", "))
  }
  
  # Determine metadata columns
  if (!is.null(meta_cols)) {
    # Convert to column names if indices
    if (is.numeric(meta_cols)) {
      meta_names <- names(df)[meta_cols]
    } else {
      meta_names <- meta_cols
    }
  } else {
    # Auto-detect: all columns except the three key columns
    meta_names <- setdiff(names(df), required)
  }
  
  # Calculate sample totals
  sample_totals <- stats::aggregate(
    df[[value_name]], 
    by = list(sample = df[[sample_name]]),
    FUN = sum, 
    na.rm = TRUE
  )
  names(sample_totals) <- c(sample_name, "total")
  
  # Calculate global mean
  global_mean <- mean(sample_totals$total, na.rm = TRUE)
  
  # Merge totals back to original data
  df_with_totals <- merge(df, sample_totals, by = sample_name, sort = FALSE)
  
  # Perform normalization
  df_with_totals[[value_name]] <- (df_with_totals[[value_name]] / df_with_totals$total) * global_mean
  
  # Remove the temporary total column
  df_with_totals$total <- NULL
  
  # Restore original column order
  df_with_totals <- df_with_totals[names(df)]
  
  df_with_totals
}
