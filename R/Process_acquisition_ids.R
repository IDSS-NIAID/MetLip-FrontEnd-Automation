#' Process sample submission data and generate acquisition IDs
#'
#' This function processes TAS sample submission data by generating unique acquisition IDs
#' based on the file name structure and selected mass spectrometry (MS) methods.
#'
#' @param submitted_sample_data A data frame containing sample submission data, with column `Submitted Sample Ids`.
#' @param selected_ms_methods A character vector of selected MS methods.
#'
#' @return A data frame with expanded sample submission data, including generated acquisition IDs.
#' @export
#'
#' @importFrom dplyr intersect slice mutate n group_by ungroup
#' @importFrom stringr str_split
#' @importFrom lubridate year
process_acquisition_ids <- function(submitted_sample_data, selected_ms_methods) {
  # take care of annoying no visible binding note
  if(FALSE)
    `Submitted Sample Ids` <- NULL
  
  # Detect the file name dynamically from the working directory
  file_list <- list.files(pattern = "*.xlsx")
  if (length(file_list) == 0) {
    stop("No Excel files found in the working directory.")
  }
  file_name <- file_list[1]  # Assuming only one Excel file is dropped at a time
  file_name <- gsub(" ", "_", file_name) # Replace spaces with _
  file_name <- gsub("-", "_", file_name) # Replace - with _
  parsed_prefix <- str_split(file_name, "_")[[1]]  # Parses the file name on the underscore
  parsed_proj_id <- parsed_prefix[2] # Extract the requester name which is associated as project ID
  parsed_proj_date <- parsed_prefix[3] # Extract the date of request
  request_id <- parsed_prefix[5] # Extract the request ID
  
  # Define parsed acquisition identifier
  current_year <- year(Sys.Date()) %% 100 # extracts last 2-digits of year
  acq_prefix <- paste0("MLA", current_year, "_", request_id, "-") #sneaky paste to ensure the year is attached to MLA
  
  # Define all ms methods
  all_ms_methods <- c("TCM", "TBL", "LM", "SCFA", "BA", "Custom")
  
  # Filter categories based on user selection
  ms_methods <- dplyr::intersect(all_ms_methods, selected_ms_methods)
  
  # Expand the sample submission data to repeat each submitted name based on selected ms methods
  submitted_sample_data_expanded <- submitted_sample_data |>
    slice(rep(1:n(), each = length(ms_methods))) |>
    mutate(MS_method = rep(ms_methods, times = n() / length(ms_methods)),
           Date = Sys.Date())
  
  # Generate new unique IDs by appending _0001 through the appropriate count
  isl_out <- submitted_sample_data_expanded |>
    mutate(Requester = parsed_proj_id,
           Acquired_Sample_ID = paste(acq_prefix, sprintf("%05d", row_number()), sep = "_"), 
           Acquired_Sample_Name = paste(`Submitted Sample Names`, MS_method)) |> 
    relocate(c(Acquired_Sample_ID, Acquired_Sample_Name), .before = `Submitted Sample Ids`)
  
  return(isl_out)
}
