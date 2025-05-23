#' Process Plate Data
#'
#' This function processes plate metadata and acquired sample data.
#' It removes missing values, filters out QC and BLANK samples, and prepares a plate loading dataframe.
#'
#' @param plate_data Data frame containing plate metadata.
#' @param acquired_samples Data frame containing acquired sample data.
#' @return A list containing plate loading data, QC plate data, BLANK plate data, and the project ID.
#' @export
#' @importFrom dplyr arrange desc filter left_join rename select
#' @importFrom stringr str_detect
#' @importFrom tidyr drop_na
process_plate_data <- function(plate_data, acquired_samples) {
  # take care of annoying no visible binding notes
  if(FALSE)
    `Submitted Sample Ids` <- Submitted_Sample_ID <- Project_ID <- Matrix <- MS_method <-
      Batch <- Plate <- Date_Processed <- `Submitted Sample Names` <- Acquired_Sample_Name <- Notes <- NULL
  
  plate_data <- plate_data %>% 
    rename(Submitted_Sample_ID = `Submitted Sample Ids`)
  
  # Remove missing values and exclude QC & BLANK samples
  temp_2 <- plate_data %>% 
    drop_na() %>% 
    filter(!str_detect(Submitted_Sample_ID, "QC") & !str_detect(Submitted_Sample_ID, "BLANK"))
  
  qc_plate_data <- plate_data %>% filter(str_detect(Submitted_Sample_ID, "QC"))
  blank_plate_data <- plate_data %>% filter(str_detect(Submitted_Sample_ID, "BLANK"))
  
  project_id <- unique(temp_2$Project_ID)
  
  plate_loading <- acquired_samples %>% 
    rename(Submitted_Sample_ID = `Submitted Sample Ids`) %>%
    left_join(plate_data, join_by(Project_ID, Submitted_Sample_ID, Matrix)) %>% 
    arrange(desc(MS_method), Batch, Plate, Position) %>% 
    select(Project_ID, Date_Processed, Submitted_Sample_ID, `Submitted Sample Names`, Matrix, MS_method, Acquired_Sample_Name, Batch, Plate, Position, Notes)
  
  return(list(plate_loading = plate_loading, qc_plate_data = qc_plate_data, blank_plate_data = blank_plate_data, project_id = project_id))
}
