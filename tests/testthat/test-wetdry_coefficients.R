test_that("Input full_df is data.frame", {
    full_df <- readRDS(system.file("extdata", "VI_STR_data.rds", package = "rOPTRAM"))
    expect_true(inherits(full_df, "data.frame"))
})

test_that("Missing AOI file input", {
  aoi_file <- NULL
  full_df <- readRDS(system.file("extdata", "VI_STR_data.rds", package = "rOPTRAM"))
  expect_null(optram_wetdry_coefficients(full_df, aoi_file))
})

test_that("Output coefficients is a data.frame of length 4", {
  full_df <- readRDS(system.file("extdata", "VI_STR_data.rds", package = "rOPTRAM"))
  aoi_file <- system.file("extdata", "migda_aoi.gpkg", package = "rOPTRAM")
  coeffs <- optram_wetdry_coefficients(full_df, aoi_file)
  expect_true(inherits(coeffs, "data.frame"))
  expect_equal(length(coeffs), 4)
})
