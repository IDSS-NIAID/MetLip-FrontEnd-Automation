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
#' @import dplyr writexl stringr shiny DT plotly lubridate
#' 
process_acquisition_ids <- function(submitted_sample_data, selected_ms_methods) {
  # Detect the file name dynamically from the working directory
  file_list <- list.files(pattern = "*.xlsx")
  if (length(file_list) == 0) {
    stop("No Excel files found in the working directory.")
  }
  file_name <- file_list[1]  # Assuming only one Excel file is dropped at a time
  parsed_prefix <- str_split(file_name, "_")[[1]]  # Parses the file name on the underscore
  parsed_proj_id <- parsed_prefix[2] # Extract the requester name which is associated as project ID
  parsed_proj_date <- parsed_prefix[3] # Extract the date of request
  
  # Define parsed acquisition identifier
  current_year <- year(Sys.Date()) %% 100 # extracts last 2-digits of year
  acq_prefix <- paste0(parsed_proj_id, "_", parsed_proj_date, "_MLA", current_year) #sneaky paste to ensure the year is attached to MLA
  
  # Define all ms methods
  all_ms_methods <- c("TCM-F5","TCM-IP", "LM", "TBL", "SCFA", "Bile-Acids", "Custom")
  
  # Filter categories based on user selection
  ms_methods <- intersect(all_ms_methods, selected_ms_methods)
  
  # Expand the sample submission data to repeat each submitted name based on selected ms methods
  submitted_sample_data_expanded <- submitted_sample_data %>% 
    slice(rep(1:n(), each = length(ms_methods))) %>% 
    mutate(MS_method = rep(ms_methods, times = n() / length(ms_methods)),
           Date = Sys.Date())
  
  # Generate new unique IDs by appending _0001 through the appropriate count
  submitted_sample_data_expanded <- submitted_sample_data_expanded %>% 
    group_by(`Submitted Sample Ids`) %>% 
    mutate(Project_ID = parsed_proj_id,
           Acquired_Sample_ID = paste(acq_prefix, sprintf("%04d", row_number()), sep = "_")) %>%
    ungroup()
  
  return(submitted_sample_data_expanded)
}
