# required libraries
library(plumber)
library(dplyr)
library(jose)


# Run this FIRST, before launching the Shiny app.
# Paste into a dedicated R console — it will block that console while running.

pr("20260608_plumber_v8.R") %>%
  pr_set_debug(TRUE) %>%
  pr_run(port = 8000)


