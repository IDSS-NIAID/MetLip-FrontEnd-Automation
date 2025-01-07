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
  retval <- expand.grid(Project_id = project_id,
                        Subject = 1:num_subjects,
                        Condition = conditions,
                        Matrix = matrices,
                        Replicate = 1:replicates) |>

    group_by(Matrix) |>
    arrange(Subject, .by_group = TRUE) |>
    mutate(Vial = sprintf("%03d", row_number())) |>
    ungroup()

  if(!is.null(path))
    write.csv(retval, path, row.names = FALSE)

  invisible(retval)
}


#' generate_plate_meta_data
#' Function to create the plate meta data using input from the collaborator
#'
#' @param sample_meta_data A data.frame with the sample meta data
#' @param num_wells Number of wells per plate
#' @param num_blanks Number of blank wells per batch
#' @param num_qc Number of QC wells per matrix type
#' @param plates_per_batch Number of plates to use per batch
#' @param path The path to save the csv file (set to NULL to skip save)
#' @param randomize Whether to randomize the sample order if they need to be run in multiple batches
#'
#' @return A data.frame with the plate meta data is invisibly returned and a csv file is generated at `path`.
#' @export
#' @importFrom dplyr arrange case_when group_by join_by left_join mutate n row_number select ungroup
generate_plate_meta_data <- function(sample_meta_data,
                                     num_wells = 54, num_blanks = 1, num_qc = 1, plates_per_batch = 3,
                                     path = 'Plate_Metadata.csv',
                                     randomize = TRUE)
{
  # get rid of those pesky "no visible binding" errors
  if(FALSE)
    Matrix <- Batch <- Plate <- Position <- Project_id <- Vial <- merge_id <- reserve_blank <- reserve_qc <- NULL

  # sample_meta_data <- MetLipAutomation::generate_sample_meta_data('abc123', c('stim', 'control'), c('plasma', 'kidney', 'heart'), 5, 40, path = NULL)

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
    batches <- 1
    num_qc_per_batch <- num_qc * num_matrices

    sample_meta_data <- sample_meta_data |>
      mutate(merge_id = row_number(),
             Batch = 1)

  }else if(plates_per_batch * num_wells - (num_blanks + num_qc) > samples_per_matrix){ # do they all fit in one batch per matrix?

    batches <- 1:num_matrices
    num_qc_per_batch <- num_qc

    sample_meta_data <- sample_meta_data |>
      group_by(Matrix) |>
      mutate(merge_id = row_number(),
             Batch = as.integer(Matrix)) |>
      ungroup()

  }else{                                                                               # if neither, split into batches by matrix

    batches_per_matrix <- ceiling(samples_per_matrix / (plates_per_batch * num_wells - (num_blanks + num_qc)))
    batches <- 1:(num_matrices * batches_per_matrix)

    num_qc_per_batch <- num_qc

    max_samples_per_batch <- plates_per_batch * num_wells - (num_blanks + num_qc_per_batch)


    # assign samples to batches
    sample_order <- rep(batches, each = max_samples_per_batch)

    if(randomize) # randomize the order of samples
    {
      sample_order <- sample(sample_order)
    }

    sample_meta_data <- sample_meta_data |>
      group_by(Matrix) |>
      mutate(                   # /---               grab batches for this Matrix               ---\  /-- pick first n() --\
             Batch = sample_order[ceiling(sample_order / num_matrices) == unique(as.integer(Matrix))][        1:n()         ]) |>
      ungroup() |>
      group_by(Matrix, Batch) |>
      mutate(merge_id = row_number()) |>
      ungroup()

  }


  # expand list of parameters into data.frame for all batches
  retval <- expand.grid(Batch = batches,
                        Plate = 1:plates_per_batch,
                        Position = 1:num_wells,
                        Project_id = unique(sample_meta_data$Project_id)) |>

    arrange(Batch, Plate, Position) |>

    # set up for merging sample data
    mutate(reserve_blank = Plate == plates_per_batch & Position > num_wells - num_blanks,
           reserve_qc    = Plate == plates_per_batch & Position > num_wells - num_blanks - num_qc_per_batch & !reserve_blank,
           merge_id = ifelse(reserve_blank | reserve_qc, NA, 1)) # need a `cumsum` by `Batch` that will ignore `NA` values -- take care of that below


  # calculate merge_id for `retval`, ignoring `NA` values
  for(i in batches)
  {
    index <- which(retval$Batch == i & !is.na(retval$merge_id))
    retval$merge_id[index] <- cumsum(retval$merge_id[index])
  }


  # merge sample data
  retval <- left_join(retval, sample_meta_data, by = join_by(Batch, Project_id, merge_id)) |>
    mutate(Vial = case_when(reserve_blank ~ paste('BLANK', Batch, sep = ''),
                            reserve_qc    ~ paste(   'QC', Batch, sep = ''),
                            TRUE          ~ Vial)) |>
    select(Batch, Plate, Position, Project_id, Vial, Matrix)


  # write csv if desired
  if(!is.null(path))
    write.csv(retval, path, row.names = FALSE)

  invisible(retval)
}
