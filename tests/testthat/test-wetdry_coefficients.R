test_that("Input full_df is data.frame", {
    full_df <- readRDS(system.file("extdata", "VI_STR_data.rds", package = "rOPTRAM"))
    expect_true(inherits(full_df, "data.frame"))
})

test_that("Missing AOI file input", {
  # NA allowd for aoi_file name
  aoi_file <- NULL
  full_df <- readRDS(system.file("extdata", "VI_STR_data.rds", package = "rOPTRAM"))
  coeffs <- optram_wetdry_coefficients(full_df, aoi_file)
  expect_true(inherits(coeffs, "data.frame"))
  expect_equal(length(coeffs), 4)
})

test_that("aoi_file does not exist", {
  aoi_file <- "xxx.gpkg"
  full_df <- readRDS(system.file("extdata", "VI_STR_data.rds", package = "rOPTRAM"))
  coeffs <- optram_wetdry_coefficients(full_df, aoi_file)
  expect_true(inherits(coeffs, "data.frame"))
  expect_equal(length(coeffs), 4)
})

test_that("Output coefficients is a data.frame of length 4", {
  full_df <- readRDS(system.file("extdata", "VI_STR_data.rds", package = "rOPTRAM"))
  aoi_file <- system.file("extdata", "migda_aoi.gpkg", package = "rOPTRAM")
  coeffs <- optram_wetdry_coefficients(full_df, aoi_file)
  expect_true(inherits(coeffs, "data.frame"))
  expect_equal(length(coeffs), 4)
  # Check length of coeffs data.frame
  expect_null(plot_ndvi_str_cloud(full_df, coeffs[1:3],
                                aoi_name, output_dir = tempdir()))
  # Check columns in full_df
  full_df2 <- full_df[, 1:3]
  expect_null(plot_ndvi_str_cloud(full_df[1:3], coeffs,
                                aoi_name, output_dir = tempdir()))
})
