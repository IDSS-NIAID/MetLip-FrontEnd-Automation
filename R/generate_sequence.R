#' Generate SciexOS Formatted Sequence
#'
#' This function generates a randomized mass spec sequence, inserting QC and BLANK samples at specified intervals.
#'
#' @param plate_loading Data frame containing autosampler plate loading information.
#' @param qc_plate_data Data frame containing QC metadata.
#' @param blank_plate_data Data frame containing BLANK metadata.
#' @param project_id Unique project identifier.
#' @param injection_vol Numeric value specifying the injection volume.
#' @param output_dir Directory to write CSV files to. Defaults to getwd(); on Posit Connect use tempdir().
#' @return (Invisibly) a character vector of file paths written.
#' @export
#' @importFrom dplyr arrange bind_rows filter mutate n rename select tibble ungroup row_number
#' @importFrom magrittr %>%
#' @importFrom stats runif
#' @importFrom utils write.csv
#' @importFrom tibble tibble
generate_sequence <- function(plate_loading, qc_plate_data, blank_plate_data, project_id, injection_vol, output_dir = getwd()) {
  # take care of annoying no visible binding notes
  if(FALSE)
    MS_method <- Randomization <- Matrix <- Batch <- Acquired_Sample_ID <- Run_number <-
      Folder_name <- Run_name <- Plate <- Injection_vol <- Data_file <- NULL
  
  # ensure output dir exists & init collector
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  written_paths <- character(0)
  
  
  # Iterate over unique MS methods in plate_loading
  for (method in unique(plate_loading$MS_method)) {
    
    # Filter and randomize the sample order for the current method
    select_method <- plate_loading %>%
      filter(MS_method == method) %>%
      mutate(Injection_vol = injection_vol) %>% 
      mutate(Randomization = runif(n())) %>%
      arrange(Randomization) %>%
      ungroup()
    
    ms_method <- unique(select_method$MS_method)
    
    # Iterate over unique sample matrices
    for (mtx in unique(select_method$Matrix)) {
      isl_temp <- select_method %>% filter(Matrix == mtx)
      
      # Iterate over unique batches
      for (batch in unique(select_method$Batch)) {
        
        # Filter samples to only those in the current batch
        batch_samples <- isl_temp %>% filter(Batch == batch) 
        
        # Filter QC and BLANK samples for the current batch
        qc_plate_data_isl <- qc_plate_data %>% filter(Batch == batch)
        blank_plate_data_isl <- blank_plate_data %>% filter(Batch == batch)
        
        # Initialize counter for QC and Blank injection numbering
        qc_blank_counter <- 0
        
        # === ADD 4 BLANK RUNS AT THE VERY BEGINNING ===
        initial_blanks <- list()
        for (i in 1:4) {
          qc_blank_counter <- qc_blank_counter + 1
          initial_blanks[[i]] <- tibble(
            Project_ID = project_id, 
            MS_method = ms_method, 
            Submitted_Sample_ID = sprintf("Blank-%02d", qc_blank_counter),
            Acquired_Sample_ID = sprintf("Blank-%02d-%s", qc_blank_counter, mtx), 
            Matrix = mtx, 
            Batch = batch, 
            Plate = unique(blank_plate_data_isl$Plate), 
            Position = unique(blank_plate_data_isl$Position), 
            Injection_vol = injection_vol
          )
        }
        
        # === ADD INITIAL QC AND BLANK (after the 4 blanks) ===
        qc_blank_counter <- qc_blank_counter + 1
        initial_qc <- tibble(
          Project_ID = project_id, 
          MS_method = ms_method, 
          Submitted_Sample_ID = sprintf("QC-%02d", qc_blank_counter),
          Acquired_Sample_ID = sprintf("QC-%02d-%s", qc_blank_counter, mtx), 
          Matrix = mtx, 
          Batch = batch, 
          Plate = unique(qc_plate_data_isl$Plate), 
          Position = unique(qc_plate_data_isl$Position), 
          Injection_vol = injection_vol
        )
        
        qc_blank_counter <- qc_blank_counter + 1
        initial_blank <- tibble(
          Project_ID = project_id, 
          MS_method = ms_method, 
          Submitted_Sample_ID = sprintf("Blank-%02d", qc_blank_counter),
          Acquired_Sample_ID = sprintf("Blank-%02d-%s", qc_blank_counter, mtx), 
          Matrix = mtx, 
          Batch = batch, 
          Plate = unique(blank_plate_data_isl$Plate), 
          Position = unique(blank_plate_data_isl$Position), 
          Injection_vol = injection_vol
        )
        
        # Combine initial blanks + QC + blank with sample data
        full_sequence <- bind_rows(initial_blanks, initial_qc, initial_blank, isl_temp)
        
        # === INSERT QC AND BLANK AT INTERVALS OF 10 ===
        # Start inserting after initial setup (row 6 onwards)
        if (nrow(full_sequence) >= 16) {
          insert_positions <- seq(6 + 10, nrow(full_sequence), by = 10)
        } else {
          insert_positions <- integer(0)
        }
        
        for (insert_pos in insert_positions) {
          qc_blank_counter <- qc_blank_counter + 1
          
          new_qc_row <- tibble(
            Project_ID = project_id, 
            MS_method = ms_method, 
            Submitted_Sample_ID = sprintf("QC-%02d", qc_blank_counter),
            Acquired_Sample_ID = sprintf("QC-%02d-%s", qc_blank_counter, mtx), 
            Matrix = mtx, 
            Batch = batch, 
            Plate = unique(qc_plate_data_isl$Plate), 
            Position = unique(qc_plate_data_isl$Position), 
            Injection_vol = injection_vol
          )
          
          qc_blank_counter <- qc_blank_counter + 1
          new_blank_row <- tibble(
            Project_ID = project_id, 
            MS_method = ms_method, 
            Submitted_Sample_ID = sprintf("Blank-%02d", qc_blank_counter),
            Acquired_Sample_ID = sprintf("Blank-%02d-%s", qc_blank_counter, mtx), 
            Matrix = mtx, 
            Batch = batch, 
            Plate = unique(blank_plate_data_isl$Plate), 
            Position = unique(blank_plate_data_isl$Position), 
            Injection_vol = injection_vol
          )
          
          # Insert QC and BLANK at the specified interval
          full_sequence <- bind_rows(
            full_sequence[1:insert_pos, ], 
            new_qc_row, 
            new_blank_row, 
            full_sequence[(insert_pos+1):nrow(full_sequence), ]
          )
          
        } # <- end interval insert loop
        
        # === ADD FINAL QC AND BLANK AT THE VERY END ===
        qc_blank_counter <- qc_blank_counter + 1
        final_qc <- tibble(
          Project_ID = project_id, 
          MS_method = ms_method, 
          Submitted_Sample_ID = sprintf("QC-%02d", qc_blank_counter),
          Acquired_Sample_ID = sprintf("QC-%02d-%s", qc_blank_counter, mtx), 
          Matrix = mtx, 
          Batch = batch, 
          Plate = unique(qc_plate_data_isl$Plate), 
          Position = unique(qc_plate_data_isl$Position), 
          Injection_vol = injection_vol
        )
        
        qc_blank_counter <- qc_blank_counter + 1
        final_blank <- tibble(
          Project_ID = project_id, 
          MS_method = ms_method, 
          Submitted_Sample_ID = sprintf("Blank-%02d", qc_blank_counter),
          Acquired_Sample_ID = sprintf("Blank-%02d-%s", qc_blank_counter, mtx), 
          Matrix = mtx, 
          Batch = batch, 
          Plate = unique(blank_plate_data_isl$Plate), 
          Position = unique(blank_plate_data_isl$Position), 
          Injection_vol = injection_vol
        )
        
        full_sequence <- bind_rows(full_sequence, final_qc, final_blank)
        
        # Prepare output file structure
        out_isl <- full_sequence %>%
          dplyr::mutate(
            Run_number = paste("Run", row_number(), sep = "-"),
            Folder_name = paste(format(Sys.Date(), "%Y%m%d"), project_id, ms_method, sep = "_"),
            Run_name = Acquired_Sample_ID,
            Data_file = paste(Folder_name, "\\", Run_name, sep = "")
          ) %>%
          dplyr::select(Acquired_Sample_ID, MS_method, Plate, Position, Injection_vol, Matrix, Data_file) %>%
          dplyr::rename(
            `Sample Name` = Acquired_Sample_ID, 
            `MS method` = MS_method, 
            `Plate Position` = Plate, 
            `Vial Position` = Position,
            `Injection Volume` = Injection_vol, 
            `Sample Type` = Matrix, 
            `Data File` = Data_file
          )
        
        # Export the sequence as a CSV file
        fname <- paste(Sys.Date(), project_id, ms_method, "Batch", batch, "Sequence.csv", sep = "_")
        fpath <- file.path(output_dir, fname)
        utils::write.csv(out_isl, fpath, row.names = FALSE)
        written_paths <- c(written_paths, fpath)
        
      } # end for batch
    }   # end for matrix
  }     # end for method
  
  invisible(written_paths)
  
}

