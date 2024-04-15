test_that("Input full_df is data.frame", {
    full_df <- readRDS(system.file("extdata",
                                   "VI_STR_data.rds", package = "rOPTRAM"))
    expect_true(inherits(full_df, "data.frame"))
})

<<<<<<< HEAD
test_that("Missing AOI file input", {
  # NA allowd for aoi_file name
  aoi_file <- NULL
  full_df <- readRDS(system.file("extdata", "VI_STR_data.rds", package = "rOPTRAM"))
  res <- optram_wetdry_coefficients(full_df, aoi_file)
  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 2)
})

test_that("aoi_file does not exist", {
  aoi_file <- "xxx.gpkg"
  full_df <- readRDS(system.file("extdata", "VI_STR_data.rds", package = "rOPTRAM"))
  res <- optram_wetdry_coefficients(full_df, aoi_file)
  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 2)
})

=======
>>>>>>> main
test_that("Output RMSE is data.frame of 2 columns", {
  full_df <- readRDS(system.file("extdata", "VI_STR_data.rds", package = "rOPTRAM"))
  res <- optram_wetdry_coefficients(full_df,
<<<<<<< HEAD
                                    aoi_file)
=======
                                    trapezoid_method = "linear")
>>>>>>> main
  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 2)
})

test_that("Polynomial trapezoid method returns data.frame of length 6", {
  full_df <- readRDS(system.file("extdata", "VI_STR_data.rds",
                                 package = "rOPTRAM"))
  output_dir <- tempdir()
<<<<<<< HEAD
  optram_options("trapezoid_method", "polynomial", show_opts = FALSE)
  res <- optram_wetdry_coefficients(full_df, aoi_file,
=======
  res <- optram_wetdry_coefficients(full_df,
                                    trapezoid_method = "polynomial",
>>>>>>> main
                                    output_dir = output_dir)
  coeffs <- read.csv(file.path(output_dir, "coefficients_poly.csv"))
  expect_true(inherits(coeffs, "data.frame"))
  expect_equal(length(coeffs), 6)
})

<<<<<<< HEAD
test_that("Plot with edge points and density coloring", {
  full_df <- readRDS(system.file("extdata", "VI_STR_data.rds", package = "rOPTRAM"))
  aoi_file <- system.file("extdata", "lachish.gpkg", package = "rOPTRAM")
  res <- optram_wetdry_coefficients(full_df, aoi_file)
  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 2)
  optram_options("plot_density", "colors", show_opts = FALSE)
  res <- optram_wetdry_coefficients(full_df, aoi_file)
  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 2)
  optram_options("plot_density", "contours", show_opts = FALSE)
  res <- optram_wetdry_coefficients(full_df, aoi_file)
=======
test_that("Plot with edge points", {
  full_df <- readRDS(system.file("extdata",
                                 "VI_STR_data.rds", package = "rOPTRAM"))
  res <- optram_wetdry_coefficients(full_df,
                                    trapezoid_method = "linear")
>>>>>>> main
  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 2)
})

test_that("Very small vi_step causes no data points in intervals", {
<<<<<<< HEAD
  full_df <- readRDS(system.file("extdata", "VI_STR_data.rds", package = "rOPTRAM"))
  aoi_file <- system.file("extdata", "lachish.gpkg", package = "rOPTRAM")
  optram_options("vi_step", 1e-5, show_opts = FALSE)
  expect_null(optram_wetdry_coefficients(full_df, aoi_file))
=======
  full_df <- readRDS(system.file("extdata",
                                 "VI_STR_data.rds", package = "rOPTRAM"))
  vi_step <- 1e-5
  expect_null(optram_wetdry_coefficients(full_df,
                                    trapezoid_method = "linear",
                                    vi_step = vi_step))
>>>>>>> main
})
