#' Generate SciexOS Formatted Sequence
#'
#' This function generates a randomized mass spec sequence, inserting QC and BLANK samples at specified intervals.
#'
#' @param plate_loading Data frame containing autosampler plate loading information.
#' @param qc_plate_data Data frame containing QC metadata.
#' @param blank_plate_data Data frame containing BLANK metadata.
#' @param project_id Unique project identifier.
#' @param injection_vol Numeric value specifying the injection volume.
#' @param lc_methods Named character vector mapping each MS method to its LC method (e.g. c(TCMU = "HILIC_pos")). Optional.
#' @param qc_blank_interval Integer. Insert a QC + blank after every N samples. Default 10.
#' @param group_by_cols Character vector of column names to randomize within. Empty = simple shuffle of all samples in the batch. When supplied, the order of groups is shuffled AND samples within each group are shuffled.
#' @param level_orders Optional named list mapping a grouping column name to an ordered character vector of its levels, e.g. list(Timepoint = c("Pre","Day1","Day7")). For any grouping column present in this list, groups run in the specified level order (deterministic) instead of being shuffled; samples are still shuffled WITHIN each group. Ordering is applied per-variable and nested in the order the columns appear in group_by_cols. Columns not listed here keep the default shuffled-group behavior. Levels not listed for a given column are appended after the specified ones, in their existing order.
#' @param qc_plate_override Optional. Override the QC plate number.
#' @param qc_position_override Optional. Override the QC vial position.
#' @param blank_plate_override Optional. Override the blank plate number.
#' @param blank_position_override Optional. Override the blank vial position.
#' @param output_dir Directory to write CSV files to. Defaults to getwd(); on Posit Connect use tempdir().
#' @return (Invisibly) a character vector of file paths written.
#' @export
#' @importFrom dplyr arrange bind_rows filter mutate n rename select slice tibble ungroup row_number desc across all_of distinct left_join
#' @importFrom magrittr %>%
#' @importFrom stats runif
#' @importFrom utils write.csv
#' @importFrom tibble tibble
generate_sequence <- function(plate_loading, qc_plate_data, blank_plate_data,
                              project_id, injection_vol,
                              lc_methods = character(),
                              qc_blank_interval = 10,
                              group_by_cols = character(),
                              level_orders = list(),
                              qc_plate_override       = NULL,
                              qc_position_override    = NULL,
                              blank_plate_override    = NULL,
                              blank_position_override = NULL,
                              output_dir = getwd()) {
  
  # Fet seed in the settings to reproducibily generate the same sequence
  set.seed(3.141592653)
  
  # take care of annoying no visible binding notes
  if(FALSE)
    MS_method <- Randomization <- Matrix <- Batch <- Acquired_Sample_ID <- Run_number <-
      Folder_name <- Run_name <- Plate <- Injection_vol <- Data_file <- Position <- NULL
  
  # ensure output dir exists & init collector
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  written_paths <- character(0)
  
  
  # Iterate over unique MS methods in plate_loading
  for (method in unique(plate_loading$MS_method)) {
    
    # Filter to the current method (randomization happens at batch scope)
    select_method <- plate_loading %>%
      filter(MS_method == method) %>%
      mutate(Injection_vol = injection_vol) %>%
      ungroup()
    
    ms_method <- unique(select_method$MS_method)
    
    # Look up the LC method for this MS method (blank if not provided)
    lc_method <- if (!is.null(lc_methods) && ms_method %in% names(lc_methods)) {
      lc_methods[[ms_method]]
    } else {
      ""
    }
    
    # Iterate over unique sample matrices
    for (mtx in unique(select_method$Matrix)) {
      isl_temp <- select_method %>% filter(Matrix == mtx)
      
      # Iterate only over batches that exist for THIS matrix
      for (batch in unique(isl_temp$Batch)) {
        
        # Filter samples to only those in the current matrix AND batch
        batch_samples <- isl_temp %>% filter(Batch == batch)
        
        # Skip empty combinations
        if (nrow(batch_samples) == 0) next
        
        # Randomize injection order (within this method × matrix × batch)
        valid_groups <- intersect(group_by_cols, colnames(batch_samples))
        
        if (length(valid_groups) > 0) {
          # Build a per-group ordering key for each grouping column.
          #
          # For a column that has a user-specified level order (via level_orders),
          # groups run in that fixed order. Levels not listed are
          # appended after the specified ones. For a column with no specified
          # order, group order is randomized. In both cases,
          # samples are randomized WITHIN each final group combination.
          #
          order_key_cols <- character(0)
          
          for (gc in valid_groups) {
            key_col <- paste0(".order_", gc)
            
            if (!is.null(level_orders[[gc]]) && length(level_orders[[gc]]) > 0) {
              # User-specified fixed order for this column's levels.
              present_levels  <- unique(as.character(batch_samples[[gc]]))
              user_levels     <- as.character(level_orders[[gc]])
              # Keep only user levels that actually appear, then append any
              # present-but-unlisted levels so nothing is dropped.
              ordered_levels  <- c(
                user_levels[user_levels %in% present_levels],
                setdiff(present_levels, user_levels)
              )
              batch_samples[[key_col]] <-
                match(as.character(batch_samples[[gc]]), ordered_levels)
            } else {
              # No specified order: assign a random rank per distinct level so
              # this column's groups are shuffled (original behavior).
              lv         <- unique(as.character(batch_samples[[gc]]))
              rand_rank  <- setNames(sample(seq_along(lv)), lv)
              batch_samples[[key_col]] <-
                rand_rank[as.character(batch_samples[[gc]])]
            }
            order_key_cols <- c(order_key_cols, key_col)
          }
          
          # Shuffle samples within each final group combination, then arrange by
          # the nested ordering keys (fixed and/or random) followed by the
          # within-group random draw.
          batch_samples <- batch_samples %>%
            mutate(.within = runif(n())) %>%
            arrange(across(all_of(order_key_cols)), .within) %>%
            select(-all_of(order_key_cols), -.within)
        } else {
          # No grouping columns provided: simple randomization of all samples in the batch
          batch_samples <- batch_samples %>%
            mutate(.rand = runif(n())) %>%
            arrange(.rand) %>%
            select(-.rand)
        }
        
        # Filter QC and BLANK samples for the current batch.
        # Use the user-supplied override if given, otherwise default to the
        # position at the very end of the last plate (plate 3, highest vial position).
        qc_plate_data_isl    <- qc_plate_data %>% filter(Batch == batch)
        blank_plate_data_isl <- blank_plate_data %>% filter(Batch == batch)
        
        qc_sel <- qc_plate_data_isl %>%
          arrange(desc(Plate), desc(Position)) %>% dplyr::slice(1)
        blank_sel <- blank_plate_data_isl %>%
          arrange(desc(Plate), desc(Position)) %>% dplyr::slice(1)
        
        qc_plate       <- if (!is.null(qc_plate_override))       qc_plate_override       else qc_sel$Plate
        qc_position    <- if (!is.null(qc_position_override))    qc_position_override    else qc_sel$Position
        blank_plate    <- if (!is.null(blank_plate_override))    blank_plate_override    else blank_sel$Plate
        blank_position <- if (!is.null(blank_position_override)) blank_position_override else blank_sel$Position
        
        # Separate counters so QC and Blank each number sequentially
        qc_counter    <- 0
        blank_counter <- 0
        
        # Add 4 BLANK Runs at the beginning
        initial_blanks <- list()
        for (i in 1:4) {
          blank_counter <- blank_counter + 1
          initial_blanks[[i]] <- tibble(
            Project_ID = project_id,
            MS_method = ms_method,
            Submitted_Sample_ID = sprintf("Blank-%02d", blank_counter),
            Acquired_Sample_ID = sprintf("Blank-%02d-%s", blank_counter, mtx),
            Matrix = mtx,
            Batch = batch,
            Plate = blank_plate,
            Position = blank_position,
            Injection_vol = injection_vol
          )
        }
        
        # Add initial QC and blank (after the initial 4 blanks)
        qc_counter <- qc_counter + 1
        initial_qc <- tibble(
          Project_ID = project_id,
          MS_method = ms_method,
          Submitted_Sample_ID = sprintf("QC-%02d", qc_counter),
          Acquired_Sample_ID = sprintf("QC-%02d-%s", qc_counter, mtx),
          Matrix = mtx,
          Batch = batch,
          Plate = qc_plate,
          Position = qc_position,
          Injection_vol = injection_vol
        )
        
        blank_counter <- blank_counter + 1
        initial_blank <- tibble(
          Project_ID = project_id,
          MS_method = ms_method,
          Submitted_Sample_ID = sprintf("Blank-%02d", blank_counter),
          Acquired_Sample_ID = sprintf("Blank-%02d-%s", blank_counter, mtx),
          Matrix = mtx,
          Batch = batch,
          Plate = blank_plate,
          Position = blank_position,
          Injection_vol = injection_vol
        )
        
        # Combine initial blanks + QC + blank with the matrix/batch sample data
        full_sequence <- bind_rows(initial_blanks, initial_qc, initial_blank, batch_samples)
        
        # INSERT QC AND BLANK AT THE CHOSEN INTERVAL
        # Positions are computed on the original length; `offset` accounts for the
        # 2 rows added each iteration so later inserts stay aligned.
        # Setup rows = 6 (4 blanks + 1 QC + 1 blank); first insert after the
        # first full interval of samples.
        first_insert <- 6 + qc_blank_interval
        if (nrow(full_sequence) > first_insert) {
          insert_positions <- seq(first_insert, nrow(full_sequence), by = qc_blank_interval)
        } else {
          insert_positions <- integer(0)
        }
        
        offset <- 0
        for (insert_pos in insert_positions) {
          adjusted_pos <- insert_pos + offset
          
          qc_counter <- qc_counter + 1
          new_qc_row <- tibble(
            Project_ID = project_id,
            MS_method = ms_method,
            Submitted_Sample_ID = sprintf("QC-%02d", qc_counter),
            Acquired_Sample_ID = sprintf("QC-%02d-%s", qc_counter, mtx),
            Matrix = mtx,
            Batch = batch,
            Plate = qc_plate,
            Position = qc_position,
            Injection_vol = injection_vol
          )
          
          blank_counter <- blank_counter + 1
          new_blank_row <- tibble(
            Project_ID = project_id,
            MS_method = ms_method,
            Submitted_Sample_ID = sprintf("Blank-%02d", blank_counter),
            Acquired_Sample_ID = sprintf("Blank-%02d-%s", blank_counter, mtx),
            Matrix = mtx,
            Batch = batch,
            Plate = blank_plate,
            Position = blank_position,
            Injection_vol = injection_vol
          )
          
          # Aavoid reverse-range duplication when at the end
          tail_rows <- if (adjusted_pos < nrow(full_sequence)) {
            full_sequence[(adjusted_pos + 1):nrow(full_sequence), ]
          } else {
            full_sequence[0, ]
          }
          
          # Insert QC and BLANK at the adjusted interval
          full_sequence <- bind_rows(
            full_sequence[1:adjusted_pos, ],
            new_qc_row,
            new_blank_row,
            tail_rows
          )
          
          offset <- offset + 2
        } # end interval insert loop
        
        # Add QC and BLANK at the end of the sequence
        qc_counter <- qc_counter + 1
        final_qc <- tibble(
          Project_ID = project_id,
          MS_method = ms_method,
          Submitted_Sample_ID = sprintf("QC-%02d", qc_counter),
          Acquired_Sample_ID = sprintf("QC-%02d-%s", qc_counter, mtx),
          Matrix = mtx,
          Batch = batch,
          Plate = qc_plate,
          Position = qc_position,
          Injection_vol = injection_vol
        )
        
        blank_counter <- blank_counter + 1
        final_blank <- tibble(
          Project_ID = project_id,
          MS_method = ms_method,
          Submitted_Sample_ID = sprintf("Blank-%02d", blank_counter),
          Acquired_Sample_ID = sprintf("Blank-%02d-%s", blank_counter, mtx),
          Matrix = mtx,
          Batch = batch,
          Plate = blank_plate,
          Position = blank_position,
          Injection_vol = injection_vol
        )
        
        full_sequence <- bind_rows(full_sequence, final_qc, final_blank)
        
        # Prepare output file structure
        # Column order: Sample Name, MS Method, LC Method, Rack Type, Plate Type,
        #               Plate Position, Vial Position, Injection Volume, Sample Type, Data File
        out_isl <- full_sequence %>%
          dplyr::mutate(
            Run_number   = paste("Run", row_number(), sep = "-"),
            Folder_name  = paste(format(Sys.Date(), "%Y%m%d"), project_id, ms_method, sep = "_"),
            Run_name     = Acquired_Sample_ID,
            Data_file    = paste(Folder_name, "\\", Run_name, sep = ""),
            `LC Method`   = lc_method,
            `Rack Type`   = "SIL-40 Drawer",
            `Plate Type`  = "1.5mL VT54 (54 vial)",
            `Sample Type` = "UNKNOWN"
          ) %>%
          dplyr::rename(
            `Sample Name`      = Acquired_Sample_ID,
            `MS Method`        = MS_method,
            `Plate Position`   = Plate,
            `Vial Position`    = Position,
            `Injection Volume` = Injection_vol,
            `Data File`        = Data_file
          ) %>%
          dplyr::select(
            `Sample Name`, `MS Method`, `LC Method`, `Rack Type`, `Plate Type`,
            `Plate Position`, `Vial Position`, `Injection Volume`, `Sample Type`, `Data File`
          )
        
        # Export the sequence as a CSV file (filename includes matrix to keep
        # method × matrix × batch combinations distinct)
        fname <- paste(Sys.Date(), project_id, ms_method, mtx, "Batch", batch,
                       "Sequence.csv", sep = "_")
        fpath <- file.path(output_dir, fname)
        utils::write.csv(out_isl, fpath, row.names = FALSE)
        written_paths <- c(written_paths, fpath)
        
      } # end for batch
    }   # end for matrix
  }     # end for method
  
  invisible(written_paths)
  
}