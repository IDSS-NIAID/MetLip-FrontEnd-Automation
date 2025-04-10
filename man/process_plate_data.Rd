% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_plate_data.R
\name{process_plate_data}
\alias{process_plate_data}
\title{Process Plate Data}
\usage{
process_plate_data(plate_data, acquired_samples)
}
\arguments{
\item{plate_data}{Data frame containing plate metadata.}

\item{acquired_samples}{Data frame containing acquired sample data.}
}
\value{
A list containing plate loading data, QC plate data, BLANK plate data, and the project ID.
}
\description{
This function processes plate metadata and acquired sample data.
It removes missing values, filters out QC and BLANK samples, and prepares a plate loading dataframe.
}
