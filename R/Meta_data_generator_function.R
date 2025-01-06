# Load necessary library
library(dplyr)


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
#' generate_sample_meta_data('abc123', c('stim', 'control'), c('plasma', 'kidney', 'heart'), 3, 5, path = NULL)
#' @export
#' @importFrom dplyr arrange group_by mutate row_number ungroup
generate_sample_meta_data <- function(project_id = vectorize_input("Enter Project ID: "),
                                      conditions = vectorize_input("Enter conditions (Space Separated): "),
                                      matrices = vectorize_input("Enter matrices (Space Separated): "),
                                      replicates = vectorize_input("Enter number of replicates: ", convert = as.numeric),
                                      num_subjects = vectorize_input("Enter number of subjects in the study: ", convert = as.numeric),
                                      path = "Sample_Meta_Data.csv")
{
  # expand list of parameters into data.frame
  retval <- expand.grid(Project_id = project_id,
                        Subject = 1:num_subjects,
                        Condition = conditions,
                        Matrix = matrices,
                        Replicate = 1:replicates) |>

    group_by(Matrix) |>
    arrange(Subject, .by_group = TRUE) |>
    mutate(Vial = paste('v', sprintf("%03d", row_number()), sep = "_")) |>
    ungroup()

  if(!is.null(path))
    write.csv(retval, path, row.names = FALSE)

  invisible(retval)
}
