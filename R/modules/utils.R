#' vectorize_input
#' Request user input and turn the resulting character-delimited string in to a vector
#'
#' @param string A character-delimited string
#' @param delim The delimiter used to separate the string
#' @param convert A function to convert the resulting vector
#'
#' @return A vector of strings
#' @export
vectorize_input <- function(string, delim = " ", convert = NULL)
{
  retval <- readline(string) |>
    strsplit(delim) |>
    unlist()

  if(!is.null(convert)) {
    retval <- convert(retval)
  }

  return(retval)
}