# Meta data generator functions


#' generate_sample_meta_data
#' Function to create the sample meta data using input from the collaborator
#'
#' @param project_id A unique identifier for the project
#' @param conditions A vector of conditions in the study
#' @param matrices A vector of sample types used in the study
#' @param replicates The number of replicates for each subject
#' @param num_subjects The number of subjects in the study
#' @param path The path to save the csv file (set to NULL to skip save)
#'
#' @return A data.frame with the sample meta data is invisibly returned and a csv file is generated at `path`.
#' @examples
#' generate_sample_meta_data(project_id = 'abc123',
#'                           conditions = c('stim', 'control'),
#'                           matrices = c('plasma', 'kidney', 'heart'),
#'                           replicates = 3,
#'                           num_subjects = 5,
#'                           path = NULL)
#' @export
#' @importFrom dplyr arrange group_by mutate row_number ungroup
#' @importFrom utils write.csv
generate_sample_meta_data <- function(project_id = vectorize_input("Enter Project ID: "),
                                      conditions = vectorize_input("Enter conditions (Comma Separated): ", ','),
                                      matrices = vectorize_input("Enter matrices (Space Separated): "),
                                      replicates = vectorize_input("Enter number of replicates: ", convert = as.numeric),
                                      num_subjects = vectorize_input("Enter number of subjects in the study: ", convert = as.numeric),
                                      path = "Sample_Meta_Data.csv")
{
  # get rid of those pesky "no visible binding" errors
  if(FALSE)
    Subject <- Matrix <- NULL
    

  # expand list of parameters into data.frame
  retval <- expand.grid(Project_ID = project_id,
                        Subject = 1:num_subjects,
                        Condition = conditions,
                        Matrix = matrices,
                        Replicate = 1:replicates) |>

    group_by(Matrix) |>
    arrange(Subject, .by_group = TRUE) |>
    mutate(`Submitted Sample Ids` = sprintf("%03d", row_number())) |>
    ungroup()

  if(!is.null(path))
    write.csv(retval, path, row.names = FALSE)

  invisible(retval)
}


#' generate_plate_meta_data
#' Function to create the plate meta data using input from the collaborator
#'
#' @param sample_meta_data A data.frame with the sample meta data (see Details)
#' @param num_wells Number of wells per plate
#' @param num_blanks Number of blank wells per batch
#' @param num_qc Number of QC wells per matrix type
#' @param plates_per_batch Number of plates to use per batch
#' @param path The path to save the csv file (set to NULL to skip save)
#' @param randomize Logical, whether to randomize the sample order if they need to be run in multiple batches
#' @param seed Integer, the seed to use for randomization
#'
#' @details The `sample_meta_data` data.frame should have the following columns: `Project_id`, `Submitted Sample Ids`, and `Matrix`.
#' Other columns are allowed but will be ignored.
#'
#' @return A data.frame with the plate meta data is invisibly returned and a csv file is generated at `path`.
#' @export
#' @importFrom dplyr arrange case_when filter group_by join_by left_join mutate n row_number select ungroup distinct bind_rows anti_join
#' @importFrom purrr map
generate_plate_meta_data <- function(sample_meta_data,
                                     num_wells = 54, num_blanks = 1, num_qc = 1, plates_per_batch = 3,
                                     path = NULL,
                                     randomize = FALSE, seed = 394875)
{
  # get rid of those pesky "no visible binding" errors
  if(FALSE)
    Matrix <- Batch <- Plate <- Position <- Project_ID <- `Submitted Sample Ids` <- merge_id <- reserve_blank <- reserve_qc <- NULL
  
  
  # assumes plate positions will not change between MS-methods
  sample_meta_data <- sample_meta_data |>
    group_by(Matrix) |> 
    dplyr::distinct(`Submitted Sample Ids`, .keep_all = TRUE)


  # assuming `sample_meta_data$Matrix` is a factor (fix if not)
  if(!is.factor(sample_meta_data$Matrix))
    sample_meta_data$Matrix <- as.factor(sample_meta_data$Matrix)


  # determine sample stats
  num_matrices <- levels(sample_meta_data$Matrix) |> length()
  num_samples <- nrow(sample_meta_data)
  samples_per_matrix <- num_samples / num_matrices


  # determine batch sizes and assign batch numbers for each row of `sample_meta_data`
  # we will assign `merge_id` to each row of `sample_meta_data` to match with `retval` below
  if(plates_per_batch * num_wells - (num_blanks + num_qc*num_matrices) > num_samples)  # do they all fit in one batch?
  {
    # one batch
    batches <- 1

    # one batch containing samples from all matrices
    batch_matrix <- unique(sample_meta_data$Matrix) |>
      as.numeric() |>
      list()

    # number of QC samples per batch
    num_qc_per_batch <- num_qc * num_matrices

    # add `merge_id` and `Batch` to `sample_meta_data`
    sample_meta_data <- sample_meta_data |>
      mutate(merge_id = row_number(),
             Batch = 1)

  }else if(plates_per_batch * num_wells - (num_blanks + num_qc) > samples_per_matrix){ # do they all fit in one batch per matrix?

    # num_matrices batches
    batches <- 1:num_matrices

    # one entry per batch listing the matrix for that batch
    batch_matrix <- unique(sample_meta_data$Matrix) |>
      as.numeric() |>
      as.list()

    # number of QC samples per batch
    num_qc_per_batch <- num_qc

    # add `merge_id` and `Batch` to `sample_meta_data`
    sample_meta_data <- sample_meta_data |>
      group_by(Matrix) |>
      mutate(merge_id = row_number(),
             Batch = as.integer(Matrix)) |>
      ungroup()

  }else{                                                                               # if neither, split into batches by matrix

    batches_per_matrix <- ceiling(samples_per_matrix / (plates_per_batch * num_wells - (num_blanks + num_qc)))

    # list of batches for each matrix
    batches <- map(1:num_matrices, ~ 1:batches_per_matrix + batches_per_matrix*(.x-1))

    # list of matrices for each batch
    batch_matrix <- map(unlist(batches), ~ ceiling(.x / batches_per_matrix))

    # number of QC samples and maximum number of samples per batch
    num_qc_per_batch <- num_qc
    max_samples_per_batch <- plates_per_batch * num_wells - (num_blanks + num_qc_per_batch)

    # assign samples to batches
    sample_order <- map(batches, ~ rep(.x, each = max_samples_per_batch))

    if(randomize) # randomize the order of samples
    {
      if(!is.null(seed))
        set.seed(seed)

      sample_order <- map(sample_order, ~ sample(.x))
    }

    # add `merge_id` and `Batch` to `sample_meta_data`
    sample_meta_data <- sample_meta_data |>
      group_by(Matrix) |>
      mutate(        #/---- grab batches for this Matrix ----\  /-- pick first n() --\
             Batch = sample_order[[unique(as.integer(Matrix))]][        1:n()         ]) |>
      ungroup() |>
      group_by(Matrix, Batch) |>
      mutate(merge_id = row_number()) |>
      ungroup()

  }


  # expand list of parameters into data.frame for all batches
  retval <- expand.grid(Batch = unlist(batches) |> unique(),
                        Plate = 1:plates_per_batch,
                        Position = 1:num_wells,
                        Project_ID = unique(sample_meta_data$Project_ID)) |>

    arrange(Batch, Plate, Position) |>

    # set up for merging sample data
    mutate(reserve_blank = Plate == plates_per_batch & Position > num_wells - num_blanks,
           reserve_qc    = Plate == plates_per_batch & Position > num_wells - num_blanks - num_qc_per_batch & !reserve_blank,
           merge_id = ifelse(reserve_blank | reserve_qc, NA, 1)) # need a `cumsum` by `Batch` that will ignore `NA` values -- take care of that below


  # calculate merge_id for `retval`, ignoring `NA` values
  for(i in unique(retval$Batch))
  {
    index <- which(retval$Batch == i & !is.na(retval$merge_id))
    retval$merge_id[index] <- cumsum(retval$merge_id[index])
  }


  # merge sample data
  retval <- left_join(retval, sample_meta_data, by = join_by(Batch, Project_ID, merge_id))

  # --- remove this chunk if the below patch works
  
  # for(i in unique(retval$Batch))
  # {
  #   retval$`Submitted Sample Ids`[retval$Batch == i & retval$reserve_blank] <- 'BLANK'
  # 
  #   index <- retval$Batch == i & retval$reserve_qc
  #   retval$`Submitted Sample Ids`[index] <- paste('QC', batch_matrix[[i]], sep = '')
  #   retval$Matrix[index] <- levels(sample_meta_data$Matrix)[batch_matrix[[i]]]
  # }

  # PATCH
  # --- begin: ensure BLANK rows avoid NA in Matrix
  for (i in unique(retval$Batch)) {
    retval$`Submitted Sample Ids`[retval$Batch == i & retval$reserve_blank] <- "BLANK"
    
    idx_qc <- retval$Batch == i & retval$reserve_qc
    retval$`Submitted Sample Ids`[idx_qc] <- paste("QC", batch_matrix[[i]], sep = "")
    retval$Matrix[idx_qc] <- levels(sample_meta_data$Matrix)[batch_matrix[[i]]]
    
    # NEW: assign Matrix for BLANK rows to a valid level (pick first level)
    idx_blank <- retval$Batch == i & retval$reserve_blank
    if (any(idx_blank)) {
      retval$Matrix[idx_blank] <- levels(sample_meta_data$Matrix)[1]
    }
  }
  # --- end
  
  # PATCH
  # --- begin: ensure every plate appeats without adding EMPTY
  for (i in unique(retval$Batch)) {
    plates_with_data <- unique(retval$Plate[retval$Batch == i & !is.na(retval$`Submitted Sample Ids`)])
    empty_plates <- setdiff(seq_len(plates_per_batch), plates_with_data)
    
    for (p in empty_plates) {
      # Prefer moving a QC; if no QC exists, move a BLANK; otherwise skip
      cand_qc   <- which(retval$Batch == i & retval$reserve_qc & !is.na(retval$`Submitted Sample Ids`))
      cand_blank<- which(retval$Batch == i & retval$reserve_blank & !is.na(retval$`Submitted Sample Ids`))
      mover <- c(cand_qc, cand_blank)
      if (length(mover) > 0) {
        mover <- mover[1L]
        retval$Plate[mover]    <- p
        retval$Position[mover] <- 1L
        # flags no longer matter downstream; we won't reuse reserve_* after this
      }
    }
  }
  # --- end
  
  
  # # PATCH
  # # --- begin: ensure empty plates are represented by a placeholder row ---
  # # Plates that would be lost after NA-filter (no non-NA Submitted Sample Ids)
  # present <- retval %>%
  #   dplyr::filter(!is.na(`Submitted Sample Ids`)) %>%
  #   dplyr::distinct(Batch, Plate)
  # 
  # all_plates <- expand.grid(
  #   Batch = unique(retval$Batch),
  #   Plate = seq_len(plates_per_batch),
  #   KEEP.OUT.ATTRS = FALSE,
  #   stringsAsFactors = FALSE
  # )
  # 
  # missing <- dplyr::anti_join(all_plates, present, by = c("Batch", "Plate"))
  # 
  # if (nrow(missing) > 0) {
  #   # one lightweight stub row per empty plate
  #   stub <- missing %>%
  #     dplyr::mutate(
  #       Position = 1L,
  #       Project_ID = unique(sample_meta_data$Project_ID)[1],
  #       `Submitted Sample Ids` = "EMPTY",
  #       Matrix = NA_character_
  #     )
  #   
  #   # Make columns align and append
  #   retval <- dplyr::bind_rows(retval, stub)
  # }
  # # --- end: ensure empty plates are represented by a placeholder row ---
  
  
  # PATCH
  # --- begin: final arrange for plate ordering and factor levels for matrix
  # keep plates in ascending order so unique(Plate) yields 1,2,3
  retval <- retval %>%
    dplyr::arrange(Batch, Plate, Position)
  
  # ensure Matrix is a factor with expected levels (even if some rows were BLANK/QC)
  retval$Matrix <- factor(retval$Matrix, levels = levels(sample_meta_data$Matrix))
  # --- end
  
  retval <- retval %>%
    dplyr::select(Batch, Plate, Position, Project_ID, `Submitted Sample Ids`, Matrix) %>%
    dplyr::filter(!is.na(`Submitted Sample Ids`))


  # write csv if desired
  if(!is.null(path))
    write.csv(retval, path, row.names = FALSE)

  invisible(retval)
}
