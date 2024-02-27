test_that("Trapezoid methods return data.frame", {
  output_dir <- tempdir()
  df <- read.csv(system.file("extdata", "trapezoid_edges.csv"),
                 package = "rOPTRAM")
  res <- linear_coefficients(df, output_dir)
  expect_s4_class(res, "data.frame")
  expect_equal(ncol(res), 5)
  res <- polynomial_coefficients(df, output_dir)
  expect_s4_class(res, "data.frame")
  expect_equal(ncol(res), 5)
  res <- exponential_coefficients(df, output_dir)
  expect_s4_class(res, "data.frame")
  expect_equal(ncol(res), 5)
})

test_that("Soil moisture raster is returned", {
  img_date <- "2023-01-25"
  VI_file <- system.file("extdata", "SAVI",
                         paste0("SAVI_", img_date, ".tif"),
                         package = "rOPTRAM")
  VI <- rast(VI_file)
  STR_file <- system.file("extdata", "STR",
                          paste0("STR_", img_date, ".tif"),
                          package = "rOPTRAM")
  STR <- rast(STR_file)
  coeffs <- system.file("extdata", "coefficients_lin.csv",
                            package = "rOPTRAM")
  expect_s4_class(linear_soil_moisture(coeffs, VI, STR),
                  "SpatRaster")
  coeffs <- system.file("extdata", "coefficients_exp.csv",
                            package = "rOPTRAM")
  expect_s4_class(exponential_soil_moisture(coeffs, VI, STR),
                  "SpatRaster")
  coeffs <- system.file("extdata", "coefficients_poly.csv",
                            package = "rOPTRAM")
  expect_s4_class(polynomial_soil_moisture(coeffs, VI, STR),
                  "SpatRaster")
})

