test_that("Input full_df is data.frame", {
    full_df <- readRDS(system.file("extdata",
                                   "VI_STR_data.rds", package = "rOPTRAM"))
    expect_true(inherits(full_df, "data.frame"))
})

test_that("Missing AOI file input", {
  # NA allowd for aoi_file name
  aoi_file <- NULL
  full_df <- readRDS(system.file("extdata", "VI_STR_data.rds", package = "rOPTRAM"))
  res <- optram_wetdry_coefficients(full_df, aoi_file,
                                       trapezoid_method = "linear")
  expect_true(inherits(res, "numeric"))
  expect_equal(length(res), 2)
})

test_that("aoi_file does not exist", {
  aoi_file <- "xxx.gpkg"
  full_df <- readRDS(system.file("extdata", "VI_STR_data.rds", package = "rOPTRAM"))
  res <- optram_wetdry_coefficients(full_df, aoi_file,
                                       trapezoid_method = "linear")
  expect_true(inherits(res, "numeric"))
  expect_equal(length(res), 2)
})

test_that("Output RMSE is list of length 2", {
  full_df <- readRDS(system.file("extdata", "VI_STR_data.rds", package = "rOPTRAM"))
  aoi_file <- system.file("extdata", "migda_9.gpkg", package = "rOPTRAM")
  res <- optram_wetdry_coefficients(full_df,
                                       aoi_file,
                                       trapezoid_method = "linear")
  expect_true(inherits(res, "numeric"))
  expect_equal(length(res), 2)
})

test_that("Polynomial trapezoid method returns data.frame of length 6", {
  full_df <- readRDS(system.file("extdata", "VI_STR_data.rds",
                                 package = "rOPTRAM"))
  aoi_file <- system.file("extdata", "migda_9.gpkg", package = "rOPTRAM")
  output_dir <- tempdir()
  res <- optram_wetdry_coefficients(full_df, aoi_file,
                                    trapezoid_method = "polynomial",
                                    output_dir = output_dir)
  coeffs <- read.csv(file.path(output_dir, "coefficients_poly.csv"))
  expect_true(inherits(coeffs, "data.frame"))
  expect_equal(length(coeffs), 6)
})
