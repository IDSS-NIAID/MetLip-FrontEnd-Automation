library(plumber)
library(jose)
library(httr)
library(jsonlite)

# ── Secrets ────────────────────────────────────────────────────────────────────
my_secret   <- charToRaw(Sys.getenv("SECRET_KEY"))
tas_api_key <- Sys.getenv("TAS_PCS_API_KEY")
tas_upn     <- Sys.getenv("TAS_PCS_UPN")

# ── TAS URL constants ──────────────────────────────────────────────────────────
tas_base_url      <- "https://tas-future.niaid.nih.gov"
tas_gen_token_url <- paste0(tas_base_url, "/api/GenerateApiToken")
tas_download_url  <- paste0(tas_base_url, "/api/Pcs/DownloadSubmittedSamples")
tas_upload_url    <- paste0(tas_base_url, "/api/Pcs/UploadAcquiredSamples")

# ── Token cache ────────────────────────────────────────────────────────────────
# Shared across all users. Refreshed automatically when within 30s of expiry.
# NOTE: open question — per-user access control to be confirmed with TAS team.
token_cache            <- new.env(parent = emptyenv())
token_cache$api_token  <- NULL
token_cache$expires_at <- 0L

# ── Helper: build & sign the identity JWT for TAS ─────────────────────────────
build_tas_identity_jwt <- function() {
  now <- as.integer(Sys.time())
  claim <- jose::jwt_claim(
    sub = tas_upn,
    aud = "NiaidTasFuture",
    exp = now + 180L,
    nbf = now - 3L
  )
  jose::jwt_encode_hmac(claim, charToRaw(tas_api_key))
}

# ── Helper: return cached token or fetch a fresh one ──────────────────────────
get_tas_token <- function() {
  now <- as.integer(Sys.time())
  if (!is.null(token_cache$api_token) && now < token_cache$expires_at - 30L) {
    return(token_cache$api_token)
  }
  
  identity_jwt <- build_tas_identity_jwt()
  res <- httr::POST(
    url = tas_gen_token_url,
    httr::add_headers(Authorization = paste("Bearer", identity_jwt))
  )
  
  if (res$status_code != 200)
    stop(paste("GenerateApiToken failed — HTTP", res$status_code))
  
  token_cache$api_token  <- trimws(httr::content(res, "text", encoding = "UTF-8"))
  token_cache$expires_at <- now + 180L
  token_cache$api_token
}

# ── Local auth helper: verify Shiny's JWT ─────────────────────────────────────
verify_local_jwt <- function(req, res) {
  auth_header <- req$HTTP_AUTHORIZATION
  if (is.null(auth_header) || !startsWith(auth_header, "Bearer ")) {
    res$status <- 401
    return(list(ok = FALSE, error = "Missing or malformed Authorization header."))
  }
  token   <- sub("Bearer ", "", auth_header, fixed = TRUE)
  decoded <- tryCatch(
    jose::jwt_decode_hmac(token, secret = my_secret),
    error = function(e) list(.__error__ = e$message)
  )
  if (!is.null(decoded$`.__error__`)) {
    res$status <- 401
    return(list(ok = FALSE, error = paste("Invalid token:", decoded$`.__error__`)))
  }
  list(ok = TRUE, claims = decoded)
}

# ── Health check ──────────────────────────────────────────────────────────────
#* @get /health
function() list(status = "OK", time = format(Sys.time()))

# ── PCS: DownloadSubmittedSamples ─────────────────────────────────────────────
# Shiny sends its local JWT with the RequestDisplayId in the `id` claim.
# Plumber validates it, fetches/uses cached TAS token, and proxies to TAS.
# Returns the xlsx binary directly to Shiny.
#* @post /pcs/download
#* @serializer contentType list(type="application/vnd.ms-excel")
function(req, res) {
  check <- verify_local_jwt(req, res)
  if (!check$ok) return(check)
  
  request_id <- check$claims$id
  if (is.null(request_id) || nchar(trimws(request_id)) == 0) {
    res$status <- 400
    return(list(error = "Missing Request Display ID in token."))
  }
  
  api_token <- tryCatch(
    get_tas_token(),
    error = function(e) { res$status <- 502; list(error = e$message) }
  )
  if (is.list(api_token)) return(api_token)
  
  # Body must be sent as raw JSON string — httr encode="json" causes TAS to 400
  raw_body <- paste0('{"RequestId":"', request_id, '"}')
  
  tas_res <- httr::POST(
    url  = tas_download_url,
    httr::add_headers(
      Authorization  = paste("Bearer", api_token),
      `Content-Type` = "application/json"
    ),
    body = raw_body
  )
  
  if (tas_res$status_code != 200) {
    res$status <- 502
    return(list(
      error   = paste("DownloadSubmittedSamples failed — HTTP", tas_res$status_code),
      details = httr::content(tas_res, "text", encoding = "UTF-8")
    ))
  }
  
  httr::content(tas_res, "raw")
}

# ── PCS: UploadAcquiredSamples ────────────────────────────────────────────────
# Shiny POSTs the upload payload as a raw JSON string in the request body.
# Plumber validates the local JWT, then forwards to TAS.
# Returns the confirmation xlsx binary to Shiny.
#* @post /pcs/upload
#* @serializer contentType list(type="application/vnd.ms-excel")
function(req, res) {
  check <- verify_local_jwt(req, res)
  if (!check$ok) return(check)
  
  api_token <- tryCatch(
    get_tas_token(),
    error = function(e) { res$status <- 502; list(error = e$message) }
  )
  if (is.list(api_token)) return(api_token)
  
  # Read the raw JSON payload forwarded from Shiny
  upload_payload <- tryCatch(
    rawToChar(req$bodyRaw),
    error = function(e) { res$status <- 400; list(error = "Could not read request body.") }
  )
  if (is.list(upload_payload)) return(upload_payload)
  
  tas_res <- httr::POST(
    url  = tas_upload_url,
    httr::add_headers(
      Authorization  = paste("Bearer", api_token),
      `Content-Type` = "application/json"
    ),
    body = upload_payload
  )
  
  if (tas_res$status_code != 200) {
    res$status <- 502
    return(list(
      error   = paste("UploadAcquiredSamples failed — HTTP", tas_res$status_code),
      details = httr::content(tas_res, "text", encoding = "UTF-8")
    ))
  }
  
  httr::content(tas_res, "raw")
}