library(plumber)
library(jose)

# Shared secret — must match the one in your Shiny app
my_secret <- charToRaw("my-123-bit-secret")

# Path to the Excel file to serve
file_path <- "TAS_lacroixis_20220420_ML1_Submitted_Samples.xlsx"

#* Health check
#* @get /health
function() {
  list(status = "OK", time = Sys.time())
}