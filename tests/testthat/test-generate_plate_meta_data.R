# test that `generate_plate_meta_data` works

test_that("generate_plate_meta_data works with three matrices in one batch", {

  # generate sample meta data with 3 matrices and 3 batches
  sample_meta_data <- generate_sample_meta_data(project_id = 'abc123',
                                                conditions = c('stim', 'control'),
                                                matrices = c('plasma', 'kidney', 'heart'),
                                                replicates = 3,
                                                num_subjects = 8,
                                                path = NULL)

  # generate plate meta data
  plate_meta_data <- generate_plate_meta_data(sample_meta_data, path = NULL)

  # check that the plate meta data has the correct number of batches
  expect_equal(length(unique(plate_meta_data$Batch)), 1)

  # check that the plate meta data has the correct number of columns
  expect_equal(ncol(plate_meta_data), 6)

  # check that the plate meta data has the correct column names
  expect_equal(colnames(plate_meta_data), c('Batch', 'Plate', 'Position', 'Project_ID', 'Submitted Sample Ids', 'Matrix'))

  # check that the plate meta data has the correct values
  expect_equal(levels(plate_meta_data$Matrix), c('plasma', 'kidney', 'heart'))
  expect_equal(unique(plate_meta_data$Plate), c(1, 2, 3))

  # check that Matrix is defined for all but blank wells (empty wells should be filtered already)
  dplyr::filter(plate_meta_data, `Submitted Sample Ids` != 'BLANK') |>
    dplyr::pull(Matrix) |>
    as.numeric() |> # convert factor to numeric
    as.logical() |> # convert to logical
    all() |>        # check if all are TRUE (if not, there are probably NAs - either way something is wrong)
    expect_equal(TRUE)
})


test_that("generate_plate_meta_data works with one batch per matrix", {

  # generate sample meta data with 3 matrices and 3 batches
  sample_meta_data <- generate_sample_meta_data(project_id = 'abc123',
                                                conditions = c('stim', 'control'),
                                                matrices = c('plasma', 'kidney', 'heart'),
                                                replicates = 5,
                                                num_subjects = 10,
                                                path = NULL)

  # generate plate meta data
  plate_meta_data <- generate_plate_meta_data(sample_meta_data, path = NULL)

  # check that the plate meta data has the correct number of batches
  expect_equal(length(unique(plate_meta_data$Batch)), 3)

  # check that the plate meta data has the correct number of columns
  expect_equal(ncol(plate_meta_data), 6)

  # check that the plate meta data has the correct column names
  expect_equal(colnames(plate_meta_data), c('Batch', 'Plate', 'Position', 'Project_ID', 'Submitted Sample Ids', 'Matrix'))

  # check that the plate meta data has the correct values
  expect_equal(levels(plate_meta_data$Matrix), c('plasma', 'kidney', 'heart'))
  expect_equal(unique(plate_meta_data$Plate), c(1, 2, 3))

  # check that Matrix is defined for all but blank wells (empty wells should be filtered already)
  dplyr::filter(plate_meta_data, `Submitted Sample Ids` != 'BLANK') |>
    dplyr::pull(Matrix) |>
    as.numeric() |> # convert factor to numeric
    as.logical() |> # convert to logical
    all() |>        # check if all are TRUE (if not, there are probably NAs - either way something is wrong)
    expect_equal(TRUE)
})


test_that("generate_plate_meta_data works with multiple batches per matrix", {

  # generate sample meta data with 3 matrices and 9 batches
  sample_meta_data <- generate_sample_meta_data(project_id = 'abc123',
                                                conditions = c('stim', 'control'),
                                                matrices = c('plasma', 'kidney', 'heart'),
                                                replicates = 5,
                                                num_subjects = 40,
                                                path = NULL)

  # generate plate meta data
  plate_meta_data <- generate_plate_meta_data(sample_meta_data, path = NULL)

  # check that the plate meta data has the correct number of batches
  expect_equal(length(unique(plate_meta_data$Batch)), 9)

  # check that the plate meta data has the correct number of columns
  expect_equal(ncol(plate_meta_data), 6)

  # check that the plate meta data has the correct column names
  expect_equal(colnames(plate_meta_data), c('Batch', 'Plate', 'Position', 'Project_ID', 'Submitted Sample Ids', 'Matrix'))

  # check that the plate meta data has the correct values
  expect_equal(levels(plate_meta_data$Matrix), c('plasma', 'kidney', 'heart'))
  expect_equal(unique(plate_meta_data$Plate), c(1, 2, 3))

  # check that Matrix is defined for all but blank wells (empty wells should be filtered already)
  dplyr::filter(plate_meta_data, `Submitted Sample Ids` != 'BLANK') |>
    dplyr::pull(Matrix) |>
    as.numeric() |> # convert factor to numeric
    as.logical() |> # convert to logical
    all() |>        # check if all are TRUE (if not, there are probably NAs - either way something is wrong)
    expect_equal(TRUE)
})
