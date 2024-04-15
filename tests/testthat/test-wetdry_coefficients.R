test_that("Input full_df is data.frame", {
    full_df <- readRDS(system.file("extdata",
                                   "VI_STR_data.rds", package = "rOPTRAM"))
    expect_true(inherits(full_df, "data.frame"))
})

test_that("Output RMSE is data.frame of 2 columns", {
  full_df <- readRDS(system.file(
    "extdata", "VI_STR_data.rds", package = "rOPTRAM"))
  optram_options("trapezoid_method","linear")
  res <- optram_wetdry_coefficients(full_df, output_dir = tempdir())
  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 2)
})

test_that("Polynomial trapezoid method returns data.frame of length 6", {
  full_df <- readRDS(system.file("extdata", "VI_STR_data.rds",
                                 package = "rOPTRAM"))
  optram_options("trapezoid_method","polynomial")
  output_dir = tempdir()
  res <- optram_wetdry_coefficients(full_df, output_dir)
  coeffs <- read.csv(file.path(output_dir, "coefficients_poly.csv"))
  expect_true(inherits(coeffs, "data.frame"))
  expect_equal(length(coeffs), 6)
})


test_that("Plot with edge points", {
  full_df <- readRDS(system.file("extdata",
                                 "VI_STR_data.rds", package = "rOPTRAM"))
  optram_options("trapezoid_method","linear")
  res <- optram_wetdry_coefficients(full_df,
                                    output_dir = tempdir())
  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 2)
})

test_that("Very small vi_step causes no data points in intervals", {
  full_df <- readRDS(system.file("extdata",
                                 "VI_STR_data.rds", package = "rOPTRAM"))
  optram_options("vi_step", 1e-5)
  optram_options("trapezoid_method","linear")
  expect_null(optram_wetdry_coefficients(full_df,
                                         output_dir = tempdir()))
})
