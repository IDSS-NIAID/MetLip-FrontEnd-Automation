#' Process sample acquisition IDs in preparation for plate meta data generation
#'
#' This function was largely reduced from the original.
#' Will be maintained in case the acquisition ID generation strategy is continued.
#' based on the file name structure and selected mass spectrometry (MS) methods.
#'
#' @param submitted_sample_data A data frame containing acquisition IDs, with column `Submitted Sample Ids`.
#'
#' @return A data frame with expanded sample submission data, including generated acquisition IDs.
#' @export
#'
#' @importFrom dplyr mutate n group_by ungroup
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
  file_name_raw <- file_list[1]  # Assuming only one Excel file is dropped at a time
  file_name <- gsub(" ", "_", file_name_raw) # Replace spaces with _
  file_name <- gsub("-", "_", file_name) # Replace - with _
  parsed_prefix <- str_split(file_name, "_")[[1]]  # Parses the file name on the underscore
  parsed_proj_id <- parsed_prefix[2] # Extract the requester name which is associated as project ID
  parsed_proj_date <- parsed_prefix[3] # Extract the date of request
  request_id <- parsed_prefix[5] # Extract the request ID
  
  # Define parsed acquisition identifier
  current_year <- year(Sys.Date()) %% 100 # extracts last 2-digits of year
  acq_prefix <- paste0("MLA", current_year, "_", request_id) #sneaky paste to ensure the year is attached to MLA
  
  
  # Define the MS methods used in this project
  ms_methods <- unique(submitted_sample_data$MS_method)
  
 
  # Generate project ID for downstream functions
  isl_keep <- submitted_sample_data |>
    mutate(Project_ID = parsed_proj_id)
  
  return(isl_keep)
}
