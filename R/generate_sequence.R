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
#' @param randomize Logical. Determines whether or not sample injection order should be randomized.
#' @param qc_plate_override Optional. Override the QC plate number.
#' @param qc_position_override Optional. Override the QC vial position.
#' @param blank_plate_override Optional. Override the blank plate number.
#' @param blank_position_override Optional. Override the blank vial position.
#' @param output_dir Directory to write CSV files to. Defaults to getwd(); on Posit Connect use tempdir().
#' @return (Invisibly) a character vector of file paths written.
#' @export
#' @importFrom dplyr arrange bind_rows filter mutate n rename select slice tibble ungroup row_number desc across all_of distinct left_join
#' @importFrom magrittr %>%
#' @importFrom stats runif setNames
#' @importFrom utils write.csv
#' @importFrom tibble tibble

generate_sequence <- function(plate_loading, qc_plate_data, blank_plate_data,
                              project_id, injection_vol,
                              lc_methods = character(),
                              sciex_names = character(),
                              qc_blank_interval = 10,
                              group_by_cols = character(),
                              level_orders = list(),
                              randomize = TRUE,
                              qc_plate_override       = NULL,
                              qc_position_override    = NULL,
                              blank_plate_override    = NULL,
                              blank_position_override = NULL,
                              output_dir = getwd()) {
  

  # seed will be set per batch from inside the loop, so every
  # batch is independent and reproducible regardless of iteration order.
  base_seed <- 3141592L
  
  # small deterministic integer hash of a string, for per-batch seeding
  .str_hash <- function(s) {
    s <- as.character(s)
    if (length(s) == 0 || is.na(s)) s <- ""
    sum(utf8ToInt(s) * seq_along(utf8ToInt(s))) %% .Machine$integer.max
  }
  
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
    
    # Look up the full SciexOS name for this MS method. The shorthand ms_method
    # still drives iteration and file names; this long name only populates the
    # "MS Method" output column. Falls back to the shorthand when not supplied.
    ms_method_sciex <- if (!is.null(sciex_names) &&
                           ms_method %in% names(sciex_names) &&
                           nzchar(sciex_names[[ms_method]])) {
      sciex_names[[ms_method]]
    } else {
      ms_method
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
        
        # Independent, reproducible randomization for each method x matrix x batch,
        set.seed(base_seed +
                   .str_hash(paste(ms_method, mtx, batch, sep = "|")))
        
        # Randomize injection order (within this method × matrix × batch).
        # When randomize = FALSE, samples keep their existing order in
        # plate_loading and grouping / level orders are ignored.
        valid_groups <- if (randomize) {
          intersect(group_by_cols, colnames(batch_samples))
        } else {
          character(0)
        }
        
        if (!randomize) {
          # No reordering: leave batch_samples exactly as received.
        } else if (length(valid_groups) > 0) {
          
          # Build a per-group ordering key for each grouping column.
          #
          # For a column that has a user-specified level order (via level_orders),
          # groups run in that fixed order. Levels not listed are appended after
          # the specified ones. For a column with no specified order, group order
          # is randomized. In both cases, samples are randomized WITHIN each final
          # group combination.
          order_key_cols <- character(0)
          
          for (gc in valid_groups) {
            key_col <- paste0(".order_", gc)
            
            has_user_order <- !is.null(level_orders[[gc]]) &&
              length(level_orders[[gc]]) > 0
            
            if (has_user_order) {
              # User-specified fixed order for this column's levels.
              present_levels  <- unique(as.character(batch_samples[[gc]]))
              present_levels  <- present_levels[!is.na(present_levels)]
              user_levels     <- as.character(level_orders[[gc]])
              
              # Warn if NONE of the saved levels appear in this batch 
              if (!any(user_levels %in% present_levels)) {
                warning(sprintf(
                  paste0("level_orders for column '%s' matches no values in ",
                         "batch '%s' (method '%s', matrix '%s'). Falling back to ",
                         "random group order for this batch. Check that the ",
                         "covariate is populated for every batch."),
                  gc, batch, ms_method, mtx))
              }
              
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
              # this column's groups are randomized across batch.
              lv         <- unique(as.character(batch_samples[[gc]]))
              rand_rank  <- setNames(sample(seq_along(lv)), lv)
              batch_samples[[key_col]] <-
                rand_rank[as.character(batch_samples[[gc]])]
            }
            order_key_cols <- c(order_key_cols, key_col)
          }
          
          # Randomize samples within each final group combination, then arrange by
          # the nested ordering keys (fixed and/or random)
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
        
        # Small helpers to build QC / Blank rows without repeating the tibble spec
        make_qc_row <- function() {
          qc_counter <<- qc_counter + 1
          tibble(
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
        }
        make_blank_row <- function() {
          blank_counter <<- blank_counter + 1
          tibble(
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
    
        
        # Setup rows: 4 blanks, then 1 QC + 1 blank
        setup_rows <- list()
        for (i in 1:4) setup_rows[[length(setup_rows) + 1]] <- make_blank_row()
        setup_rows[[length(setup_rows) + 1]] <- make_qc_row()
        setup_rows[[length(setup_rows) + 1]] <- make_blank_row()
        setup_block <- bind_rows(setup_rows)
        
        # Insert QC+blank after every `qc_blank_interval` samples,
        # but NOT after the final chunk.
        n_samp <- nrow(batch_samples)
        chunk_starts <- if (n_samp > 0) seq(1, n_samp, by = qc_blank_interval) else integer(0)
        
        sample_block_parts <- list()
        for (ci in seq_along(chunk_starts)) {
          start_i <- chunk_starts[ci]
          end_i   <- min(start_i + qc_blank_interval - 1, n_samp)
          sample_block_parts[[length(sample_block_parts) + 1]] <-
            batch_samples[start_i:end_i, ]
          
          # Insert a QC+blank only if there are MORE samples after this chunk.
          if (end_i < n_samp) {
            sample_block_parts[[length(sample_block_parts) + 1]] <- make_qc_row()
            sample_block_parts[[length(sample_block_parts) + 1]] <- make_blank_row()
          }
        }
        sample_block <- bind_rows(sample_block_parts)
        
        # Trailing QC + blank (exactly one pair at the very end)
        final_block <- bind_rows(make_qc_row(), make_blank_row())
        
        # Full sequence
        full_sequence <- bind_rows(setup_block, sample_block, final_block)
        
        # Prepare output file structure
        # Column order: Sample Name, MS Method, LC Method, Rack Type, Plate Type,
        #               Plate Position, Vial Position, Injection Volume, Sample Type, Data File
        out_isl <- full_sequence %>%
          dplyr::mutate(
            Run_number   = paste("Run", row_number(), sep = "-"),
            Folder_name  = paste(format(Sys.Date(), "%Y%m%d"), project_id, ms_method, sep = "_"),
            Run_name     = Acquired_Sample_ID,
            Data_file    = paste(Folder_name, "\\", Run_name, sep = ""),
            `MS Method`   = ms_method_sciex,
            `LC Method`   = lc_method,
            `Rack Type`   = "SIL-40 Drawer",
            `Plate Type`  = "1.5mL VT54 (54 vial)",
            `Sample Type` = "UNKNOWN"
          ) %>%
          dplyr::rename(
            `Sample Name`      = Acquired_Sample_ID,
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
