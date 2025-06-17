test_that("Trapezoid methods return data.frame", {
  output_dir <- tempdir()
  df <- read.csv(system.file("extdata", "trapezoid_edges.csv",
                 package = "rOPTRAM"))
  res <- linear_coefficients(df, output_dir)
  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 5)
  res <- polynomial_coefficients(df, output_dir)
  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 5)
  res <- exponential_coefficients(df, output_dir)
  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 5)
})

test_that("Soil moisture raster is returned for all methods", {
  img_date <- "2023-01-25"
  tile <- "_T36RXV"
  VI_file <- system.file("extdata", "NDVI",
                         paste0("NDVI_", img_date, tile, ".tif"),
                         package = "rOPTRAM")
  VI <- terra::rast(VI_file)
  STR_file <- system.file("extdata", "STR",
                          paste0("STR_", img_date, tile, ".tif"),
                          package = "rOPTRAM")
  STR <- terra::rast(STR_file)
  coeffs_csv <- system.file("extdata", "coefficients_lin.csv",
                          package = "rOPTRAM")
  coeffs <- read.csv(coeffs_csv)
  W <- linear_soil_moisture(coeffs, VI, STR)
  expect_true(inherits(W, "SpatRaster"))
  coeffs_csv <- system.file("extdata", "coefficients_exp.csv",
                          package = "rOPTRAM")
  coeffs <- read.csv(coeffs_csv)
  W <- exponential_soil_moisture(coeffs, VI, STR)
  expect_true(inherits(W, "SpatRaster"))
  coeffs_csv <- system.file("extdata", "coefficients_pol.csv",
                          package = "rOPTRAM")
  coeffs <- read.csv(coeffs_csv)
  W <- polynomial_soil_moisture(coeffs, VI, STR)
  expect_true(inherits(W, "SpatRaster"))
})


