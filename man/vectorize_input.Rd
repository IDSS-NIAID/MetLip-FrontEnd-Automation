% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{vectorize_input}
\alias{vectorize_input}
\title{vectorize_input
Request user input and turn the resulting character-delimited string in to a vector}
\usage{
vectorize_input(string, delim = " ", convert = NULL)
}
\arguments{
\item{string}{A character-delimited string}

\item{delim}{The delimiter used to separate the string}

\item{convert}{A function to convert the resulting vector}
}
\value{
A vector of strings
}
\description{
vectorize_input
Request user input and turn the resulting character-delimited string in to a vector
}
