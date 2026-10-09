#' Create a TAS API client
#'
#' @param api_key TAS API key.
#' @param upn TAS user principal name.
#' @param base_url TAS base URL.
#'
#' @return A TAS client.
#' @export
new_tas_client <- function(
  api_key = Sys.getenv("TAS_PCS_API_KEY"),
  upn = Sys.getenv("TAS_PCS_UPN"),
  base_url = "https://rtb.nih.gov"
) {
  client <- list(
    api_key = api_key,
    upn = upn,
    token_url = paste0(base_url, "/api/GenerateApiToken"),
    download_url = paste0(base_url, "/api/Pcs/DownloadSubmittedSamples"),
    upload_url = paste0(base_url, "/api/Pcs/UploadAcquiredSamples"),
    cache = new.env(parent = emptyenv())
  )
  client$cache$api_token <- NULL
  client$cache$expires_at <- 0L
  class(client) <- "tas_client"
  client
}

#' Build a signed TAS identity JWT
#'
#' @param client A TAS client.
#' @param now Current time.
#'
#' @return A signed JWT.
#' @export
build_tas_identity_jwt <- function(client, now = Sys.time()) {
  if (!nzchar(client$api_key) || !nzchar(client$upn)) {
    stop("TAS_PCS_API_KEY and TAS_PCS_UPN must be configured.", call. = FALSE)
  }
  timestamp <- as.integer(now)
  claim <- jose::jwt_claim(
    sub = client$upn,
    aud = "NiaidTasProduction",
    exp = timestamp + 180L,
    nbf = timestamp - 3L
  )
  jose::jwt_encode_hmac(claim, charToRaw(client$api_key))
}

#' Get a cached or fresh TAS API token
#'
#' @param client A TAS client.
#' @param now Current time.
#'
#' @return A TAS API token.
#' @export
get_tas_token <- function(client, now = Sys.time()) {
  timestamp <- as.integer(now)
  if (!is.null(client$cache$api_token) &&
      timestamp < client$cache$expires_at - 30L) {
    return(client$cache$api_token)
  }

  response <- httr::POST(
    url = client$token_url,
    httr::add_headers(
      Authorization = paste("Bearer", build_tas_identity_jwt(client, now))
    )
  )
  if (response$status_code != 200) {
    stop(
      paste("GenerateApiToken failed HTTP", response$status_code),
      call. = FALSE
    )
  }

  client$cache$api_token <- trimws(httr::content(
    response,
    "text",
    encoding = "UTF-8"
  ))
  client$cache$expires_at <- timestamp + 180L
  client$cache$api_token
}

#' Download submitted samples from TAS
#'
#' @param client A TAS client.
#' @param request_id TAS request display ID.
#'
#' @return An httr response.
#' @export
tas_download_submitted_samples <- function(client, request_id) {
  httr::POST(
    url = client$download_url,
    httr::add_headers(
      Authorization = paste("Bearer", get_tas_token(client)),
      `Content-Type` = "application/json"
    ),
    body = jsonlite::toJSON(list(RequestId = request_id), auto_unbox = TRUE)
  )
}

#' Upload acquired samples to TAS
#'
#' @param client A TAS client.
#' @param request_id TAS request display ID.
#' @param acquired_samples Data frame of acquired samples.
#' @param submitted_id_column Submitted sample ID column name.
#'
#' @return An httr response.
#' @export
tas_upload_acquired_samples <- function(
  client,
  request_id,
  acquired_samples,
  submitted_id_column
) {
  required_columns <- c("Acquired Sample Name", submitted_id_column, "Notes")
  missing_columns <- setdiff(required_columns, colnames(acquired_samples))
  if (length(missing_columns) > 0) {
    stop(
      paste("Missing required upload columns:", paste(missing_columns, collapse = ", ")),
      call. = FALSE
    )
  }

  samples <- lapply(seq_len(nrow(acquired_samples)), function(index) {
    list(
      AcquiredSampleName = acquired_samples[["Acquired Sample Name"]][index],
      SubmittedSampleIds = list(acquired_samples[[submitted_id_column]][index]),
      Notes = ifelse(is.na(acquired_samples[["Notes"]][index]), "", acquired_samples[["Notes"]][index])
    )
  })
  payload <- jsonlite::toJSON(
    list(RequestDisplayId = request_id, AcquiredSamples = samples),
    auto_unbox = TRUE,
    null = "null"
  )

  httr::POST(
    url = client$upload_url,
    httr::add_headers(
      Authorization = paste("Bearer", get_tas_token(client)),
      `Content-Type` = "application/json"
    ),
    body = payload
  )
}

#' Detect the submitted sample ID column
#'
#' @param data A data frame.
#'
#' @return The first supported column name, or `NA_character_`.
#' @export
detect_submitted_sample_id_column <- function(data) {
  matches <- intersect(
    c("Submitted Sample Ids", "SubmittedSampleIds", "SubmittedSampleId", "SampleId"),
    colnames(data)
  )
  if (length(matches) == 0) NA_character_ else matches[[1]]
}
