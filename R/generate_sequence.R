#' Generate SciexOS Formatted Sequence
#'
#' This function generates a randomized mass spec sequence, inserting QC and BLANK samples at specified intervals.
#'
#' @param plate_loading Data frame containing autosampler plate loading information.
#' @param qc_plate_data Data frame containing QC metadata.
#' @param blank_plate_data Data frame containing BLANK metadata.
#' @param project_id Unique project identifier.
#' @param injection_vol Numeric value specifying the injection volume.
#' @return Exports sequence CSV files for SciexOS import.
#' @export
#' @importFrom dplyr arrange bind_rows filter mutate n rename select tibble ungroup
#' @importFrom magrittr %>%
#' @importFrom stats runif
#' @importFrom utils write.csv
generate_sequence <- function(plate_loading, qc_plate_data, blank_plate_data, project_id, injection_vol) {
  # take care of annoying no visible binding notes
  if(FALSE)
    MS_method <- Randomization <- Matrix <- Batch <- Acquired_Sample_ID <- Run_number <-
      Folder_name <- Run_name <- Plate <- Injection_vol <- Data_file <- NULL
  
  # Iterate over unique MS methods in plate_loading
  for (method in unique(plate_loading$MS_method)) {
    
    # Filter and randomize the sample order for the current method
    select_method <- plate_loading %>%
      filter(MS_method == method) %>%
      mutate(Randomization = runif(n())) %>%
      arrange(Randomization) %>%
      ungroup()
    
    ms_method <- unique(select_method$MS_method)
    
    # Iterate over unique sample matrices
    for (mtx in unique(select_method$Matrix)) {
      isl_temp <- select_method %>% filter(Matrix == mtx)
      
      # Iterate over unique batches
      for (batch in unique(select_method$Batch)) {
        
        # Filter QC and BLANK samples for the current batch
        qc_plate_data_isl <- qc_plate_data %>% filter(Batch == batch)
        blank_plate_data_isl <- blank_plate_data %>% filter(Batch == batch)
        
        # Ensure QC and BLANK samples appear at the start
        new_qc_row <- tibble(
          Project_ID = project_id, MS_method = ms_method, Submitted_Sample_ID = "QC-0", 
          Acquired_Sample_ID = paste("QC", mtx, sep = "-"), Matrix = mtx, Batch = batch, 
          Plate = unique(qc_plate_data_isl$Plate), Position = unique(qc_plate_data_isl$Position), 
          Injection_vol = injection_vol
        )
        
        new_blank_row <- tibble(
          Project_ID = project_id, MS_method = ms_method, Submitted_Sample_ID = "Blank-0", 
          Acquired_Sample_ID = paste("Blank", mtx, sep = "-"), Matrix = mtx, Batch = batch, 
          Plate = unique(blank_plate_data_isl$Plate), Position = unique(blank_plate_data_isl$Position), 
          Injection_vol = injection_vol
        )
        
        # Combine QC, BLANK, and sample data into full sequence
        full_sequence <- bind_rows(new_qc_row, new_blank_row, isl_temp)
        insert_counter <- 0
        
        # Insert QC and BLANK at intervals of 10
        for (i in seq(10, nrow(full_sequence), by = 10)) {
          insert_counter <- insert_counter + 1
          
          new_qc_row <- tibble(
            Project_ID = project_id, MS_method = ms_method, Submitted_Sample_ID = paste("QC", insert_counter, sep = "-"),
            Acquired_Sample_ID = paste("QC", mtx, sep = "-"), Matrix = mtx, Batch = batch, 
            Plate = unique(qc_plate_data_isl$Plate), Position = unique(qc_plate_data_isl$Position), Injection_vol = injection_vol
          )
          
          new_blank_row <- tibble(
            Project_ID = project_id, MS_method = ms_method, Submitted_Sample_ID = paste("Blank", insert_counter, sep = "-"),
            Acquired_Sample_ID = paste("Blank", mtx, sep = "-"), Matrix = mtx, Batch = batch, 
            Plate = unique(blank_plate_data_isl$Plate), Position = unique(blank_plate_data_isl$Position), Injection_vol = injection_vol
          )
          
          # Insert QC and BLANK at the specified interval
          full_sequence <- bind_rows(full_sequence[1:i, ], new_qc_row, new_blank_row, full_sequence[(i+1):nrow(full_sequence), ])
        }
        
        # Prepare output file structure
        out_isl <- full_sequence %>%
          mutate(
            Run_number = paste("Run", row_number(), sep = "-"),
            Folder_name = paste(format(Sys.Date(), "%Y%m%d"), project_id, ms_method, sep = "_"),
            Run_name = paste(project_id, Acquired_Sample_ID, Run_number, sep = "_"),
            Data_file = paste(Folder_name, "\\", Run_name, sep = "")
          ) %>%
          select(Acquired_Sample_ID, MS_method, Plate, Position, Injection_vol, Matrix, Data_file) %>%
          rename(
            `Sample Name` = Acquired_Sample_ID, `MS method` = MS_method, `Plate Position` = Plate, `Vial Position` = Position,
            `Injection Volume` = Injection_vol, `Sample Type` = Matrix, `Data File` = Data_file
          )
        
        # Export the sequence as a CSV file
        write.csv(out_isl, paste(Sys.Date(), project_id, ms_method, "Batch", batch, "Sequence.csv", sep = "_"), row.names = FALSE)
      }
    }
  }
}
