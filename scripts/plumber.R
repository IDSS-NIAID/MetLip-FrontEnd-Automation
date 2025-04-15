library(plumber)
library(jose)

# Shared secret — must match the one in the Shiny app
# This will come from Phil 20 or 30 characters, will be updated periodically.
my_secret <- charToRaw("my-123-bit-secret")

# Path to the Excel file to serve
file_path <- "TAS_lacroixis_20220420_ML1_Submitted_Samples.xlsx"

#* Health check
#* @get /health
function() {
  list(status = "OK", time = Sys.time())
}

#* Return Excel file if JWT is valid
#* @post /secure-id-check
#* @serializer contentType list(type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
function(req, res) {
  auth_header <- req$HTTP_AUTHORIZATION
  
  if (is.null(auth_header) || !startsWith(auth_header, "Bearer ")) {
    res$status <- 401
    return(list(error = "Missing or malformed Authorization header."))
  }
  
  token <- sub("Bearer ", "", auth_header)
  
  decoded <- tryCatch({
    jwt_decode_hmac(token, secret = my_secret)
  }, error = function(e) {
    res$status <- 401
    return(list(error = "Invalid token", details = e$message))
  })
  
  # Return Excel file
  readBin(file_path, "raw", n = file.info(file_path)$size)
}


# run this to establish API
pr("plumber.R") %>% 
       pr_set_debug(TRUE) %>% 
       pr_run(port = 8000)