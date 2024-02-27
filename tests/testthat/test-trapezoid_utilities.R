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

test_that("Check for correct number of coefficients", {
  img_date <- "2023-01-25"
  VI_file <- system.file("extdata", "SAVI",
                         paste0("SAVI_", img_date, ".tif"),
                         package = "rOPTRAM")
  VI <- terra::rast(VI_file)
  STR_file <- system.file("extdata", "STR",
                          paste0("STR_", img_date, ".tif"),
                          package = "rOPTRAM")
  STR <- terra::rast(STR_file)
  coeffs_csv <- system.file("extdata", "coefficients_lin.csv",
                            package = "rOPTRAM")
  coeffs <- read.csv(coeffs_csv)[1:3]
  expect_null(linear_soil_moisture(coeffs, VI, STR))

  coeffs_csv <- system.file("extdata", "coefficients_exp.csv",
                            package = "rOPTRAM")
  coeffs <- read.csv(coeffs_csv)[1:3]
  expect_null(exponential_soil_moisture(coeffs, VI, STR))

  coeffs_csv <- system.file("extdata", "coefficients_poly.csv",
                            package = "rOPTRAM")
  coeffs <- read.csv(coeffs_csv)[1:5]
  expect_null(polynomial_soil_moisture(coeffs, VI, STR))
})

test_that("Soil moisture raster is returned", {
  img_date <- "2023-01-25"
  VI_file <- system.file("extdata", "SAVI",
                         paste0("SAVI_", img_date, ".tif"),
                         package = "rOPTRAM")
  VI <- terra::rast(VI_file)
  STR_file <- system.file("extdata", "STR",
                          paste0("STR_", img_date, ".tif"),
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
  coeffs_csv <- system.file("extdata", "coefficients_poly.csv",
                          package = "rOPTRAM")
  coeffs <- read.csv(coeffs_csv)
  W <- polynomial_soil_moisture(coeffs, VI, STR)
  expect_true(inherits(W, "SpatRaster"))
})


