#' Append a user-entered project name as a new column
#'
#' @param df A data.frame (or tibble) of your preprocessed rows.
#' @param project_name Length-1 character. The name typed by the user.
#' @param col Column name to write. Default "Project_Name".
#' @param overwrite If TRUE and the column already exists, overwrite it.
#'
#' @return The same data.frame with a new column repeating \code{project_name} for all rows.
#' @export
#' @examples
#' df <- data.frame(id = 1:3)
#' append_project_name(df, "ABC123")
#'
#' @importFrom dplyr mutate
append_project_name <- function(df, project_name, col = "Project_ID", overwrite = FALSE) {
  stopifnot(is.data.frame(df))
  if (length(project_name) != 1L || !is.character(project_name))
    stop("`project_name` must be a length-1 character.", call. = FALSE)
  
  prj <- trimws(project_name)
  if (!nzchar(prj))
    stop("Project name cannot be empty.", call. = FALSE)
  
  if (col %in% names(df) && !overwrite) {
    warning(sprintf("Column '%s' already exists; set `overwrite = TRUE` to replace."), call. = FALSE)
    return(df)
  }
  
  df[[col]] <- prj
  df
}
