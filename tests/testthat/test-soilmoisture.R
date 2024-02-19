test_that("Coefficients file exists", {
  VI_dir <- system.file("extdata", "SAVI", package = "rOPTRAM")
  STR_dir <- system.file("extdata", "STR", package = "rOPTRAM")
  coeffs_file <- system.file("extdata", "xxx.csv", package = "rOPTRAM")
  img_date <- "2023-03-16"
  expect_null(optram_calculate_soil_moisture(img_date = img_date,
        VI_dir = VI_dir, STR_dir = STR_dir, coeffs_file = coeffs_file))
})

test_that("Correct image date format", {
  VI_dir <- system.file("extdata", "SAVI", package = "rOPTRAM")
  STR_dir <- system.file("extdata", "STR", package = "rOPTRAM")
  coeffs_file <- system.file("extdata", "coefficients.csv",
                              package = "rOPTRAM")
  img_date <- "2023-13-16"  # illegal date
  expect_null(optram_calculate_soil_moisture(img_date = img_date,
        VI_dir = VI_dir, STR_dir = STR_dir, coeffs_file = coeffs_file))
})


test_that("Input NDVI directory exists", {
  VI_dir <- system.file("extdata", "XXX", package = "rOPTRAM")
  STR_dir <- system.file("extdata", "STR", package = "rOPTRAM")
  coeffs_file <- system.file("extdata", "coefficients.csv",
                              package = "rOPTRAM")
  img_date <- "2023-03-16"
  expect_null(optram_calculate_soil_moisture(img_date = img_date,
        VI_dir = VI_dir, STR_dir = STR_dir, coeffs_file = coeffs_file))
})


test_that("Input STR directory exists", {
  VI_dir <- system.file("extdata", "SAVI", package = "rOPTRAM")
  STR_dir <- system.file("extdata", "XXX", package = "rOPTRAM")
  coeffs_file <- system.file("extdata", "coefficients.csv",
                              package = "rOPTRAM")
  img_date <- "2023-03-16"
  expect_null(optram_calculate_soil_moisture(img_date = img_date,
        VI_dir = VI_dir, STR_dir = STR_dir, coeffs_file = coeffs_file))
})

test_that("Length of lists of VI and STR", {
  img_date <- "2023-03-16"
  VI_dir <- system.file("extdata", "SAVI", package = "rOPTRAM")
  coeffs_file <- system.file("extdata", "coefficients.csv",
                              package = "rOPTRAM")
  # Empty directory, just for testing
  STR_dir <- system.file("extdata", "STR2", package = "rOPTRAM")

  expect_null(optram_calculate_soil_moisture(img_date = img_date,
        VI_dir = VI_dir, STR_dir = STR_dir, coeffs_file = coeffs_file))

  VI_dir <- system.file("extdata", "SAVI2", package = "rOPTRAM")
  STR_dir <- system.file("extdata", "STR", package = "rOPTRAM")

  expect_null(optram_calculate_soil_moisture(img_date = img_date,
        VI_dir = VI_dir, STR_dir = STR_dir, coeffs_file = coeffs_file))
})

test_that("Output is a terra SpatRaster", {
  VI_dir <- system.file("extdata", "SAVI", package = "rOPTRAM")
  STR_dir <- system.file("extdata", "STR", package = "rOPTRAM")
  coeffs_file <- system.file("extdata", "coefficients.csv",
                              package = "rOPTRAM")
  img_date <- "2023-03-16"
  W_rast <- optram_calculate_soil_moisture(img_date = img_date,
        VI_dir = VI_dir,
        STR_dir = STR_dir,
        coeffs_file = coeffs_file,
        trapezoid_method = "linear")
  expect_true(inherits(W_rast, "SpatRaster"))
})

test_that("Output is a terra SpatRaster, for exponential trapezoid_method", {
  VI_dir <- system.file("extdata", "SAVI", package = "rOPTRAM")
  STR_dir <- system.file("extdata", "STR", package = "rOPTRAM")
  coeffs_file <- system.file("extdata", "coefficients.csv",
                              package = "rOPTRAM")
  img_date <- "2023-03-16"
  W_rast <- optram_calculate_soil_moisture(img_date = img_date,
        VI_dir = VI_dir,
        STR_dir = STR_dir,
        coeffs_file = coeffs_file,
        trapezoid_method = "exponential")
  expect_true(inherits(W_rast, "SpatRaster"))
})

test_that("A correct trapezoid method is given", {
 VI_dir <- system.file("extdata", "SAVI", package = "rOPTRAM")
  STR_dir <- system.file("extdata", "STR", package = "rOPTRAM")
  coeffs_file <- system.file("extdata", "coefficients.csv", package = "rOPTRAM")
  img_date <- "2023-03-16"
  expect_error(optram_calculate_soil_moisture(img_date = img_date,
        VI_dir = VI_dir,
        STR_dir = STR_dir,
        coeffs_file = coeffs_file,
        trapezoid_method = "non-linear"))
})
