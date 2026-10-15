# Create a TAS API client with a token cache scoped to this R process.
new_tas_client <- function(
  api_key = Sys.getenv("TAS_PCS_API_KEY"),
  upn = Sys.getenv("TAS_PCS_UPN"),
  base_url = "https://rtb.nih.gov"
) {
  gen_token_url <- paste0(base_url, "/api/GenerateApiToken")
  download_url <- paste0(base_url, "/api/Pcs/DownloadSubmittedSamples")
  upload_url <- paste0(base_url, "/api/Pcs/UploadAcquiredSamples")

  token_cache <- new.env(parent = emptyenv())
  token_cache$api_token <- NULL
  token_cache$expires_at <- 0L

  build_tas_identity_jwt <- function() {
    now <- as.integer(Sys.time())
    claim <- jose::jwt_claim(
      sub = upn,
      aud = "NiaidTasProduction",
      exp = now + 180L,
      nbf = now - 3L
    )
    jose::jwt_encode_hmac(claim, charToRaw(api_key))
  }

  get_tas_token <- function() {
    now <- as.integer(Sys.time())
    if (!is.null(token_cache$api_token) &&
        now < token_cache$expires_at - 30L) {
      return(token_cache$api_token)
    }

    identity_jwt <- build_tas_identity_jwt()
    response <- httr::POST(
      url = gen_token_url,
      httr::add_headers(Authorization = paste("Bearer", identity_jwt))
    )
    if (response$status_code != 200) {
      stop(paste("GenerateApiToken failed HTTP", response$status_code))
    }

    token_cache$api_token <- trimws(
      httr::content(response, "text", encoding = "UTF-8")
    )
    token_cache$expires_at <- now + 180L
    token_cache$api_token
  }

  list(
    download = function(request_id) {
      api_token <- get_tas_token()
      raw_body <- paste0('{"RequestId":"', request_id, '"}')
      httr::POST(
        url = download_url,
        httr::add_headers(
          Authorization = paste("Bearer", api_token),
          `Content-Type` = "application/json"
        ),
        body = raw_body
      )
    },
    upload = function(upload_payload) {
      api_token <- get_tas_token()
      httr::POST(
        url = upload_url,
        httr::add_headers(
          Authorization = paste("Bearer", api_token),
          `Content-Type` = "application/json"
        ),
        body = upload_payload
      )
    },
    detect_sub_id_col = function(df) {
      intersect(
        c("Submitted Sample Ids", "SubmittedSampleIds",
          "SubmittedSampleId", "SampleId"),
        colnames(df)
      )[1]
    }
  )
}
