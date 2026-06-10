# required libraries
library(plumber)
library(dplyr)
library(jose)

pr("plumber.R") %>%
  pr_set_debug(TRUE) %>%
  pr_run(port = 8000)


