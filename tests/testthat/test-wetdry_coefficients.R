
test_that("Output RMSE is data.frame of 2 columns", {
  full_df <- readRDS(system.file("extdata", "VI_STR_data.rds", package = "rOPTRAM"))
  res <- optram_wetdry_coefficients(full_df,
                                    trapezoid_method = "linear")
  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 2)
})

test_that("Polynomial trapezoid method returns data.frame of length 6", {
  full_df <- readRDS(system.file("extdata", "VI_STR_data.rds",
                                 package = "rOPTRAM"))
  output_dir <- tempdir()
  res <- optram_wetdry_coefficients(full_df,
                                    trapezoid_method = "polynomial",
                                    output_dir = output_dir)
  coeffs <- read.csv(file.path(output_dir, "coefficients_poly.csv"))
  expect_true(inherits(coeffs, "data.frame"))
  expect_equal(length(coeffs), 6)
})

test_that("Plot with edge points", {
  full_df <- readRDS(system.file("extdata",
                                 "VI_STR_data.rds", package = "rOPTRAM"))
  res <- optram_wetdry_coefficients(full_df,
                                    trapezoid_method = "linear")
  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 2)
})

test_that("Very small vi_step causes no data points in intervals", {
  full_df <- readRDS(system.file("extdata",
                                 "VI_STR_data.rds", package = "rOPTRAM"))
  vi_step <- 1e-5
  expect_null(optram_wetdry_coefficients(full_df,
                                    trapezoid_method = "linear",
                                    vi_step = vi_step))
})
