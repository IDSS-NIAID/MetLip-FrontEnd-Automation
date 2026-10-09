test_that("TAS client stores configuration and an isolated token cache", {
  client <- new_tas_client(api_key = "key", upn = "user@example.org")

  expect_s3_class(client, "tas_client")
  expect_equal(client$upn, "user@example.org")
  expect_null(client$cache$api_token)
  expect_equal(client$cache$expires_at, 0L)
})

test_that("TAS JWT creation requires configured credentials", {
  client <- new_tas_client(api_key = "", upn = "")

  expect_error(
    build_tas_identity_jwt(client),
    "TAS_PCS_API_KEY and TAS_PCS_UPN must be configured"
  )
})

test_that("submitted sample ID detection supports known column names", {
  expect_identical(
    detect_submitted_sample_id_column(
      data.frame("Submitted Sample Ids" = "sample-1", check.names = FALSE)
    ),
    "Submitted Sample Ids"
  )
  expect_true(is.na(detect_submitted_sample_id_column(data.frame(sample = "sample-1"))))
})
