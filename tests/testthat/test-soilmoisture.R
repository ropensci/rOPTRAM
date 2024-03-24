test_that("Coefficients file exists", {
  VI_dir <- system.file("extdata", "NDVI", package = "rOPTRAM")
  STR_dir <- system.file("extdata", "STR", package = "rOPTRAM")
  data_dir <- system.file("extdata", "STR2", package = "rOPTRAM")
  img_date <- "2023-02-19"
  expect_null(optram_calculate_soil_moisture(img_date = img_date,
        VI_dir = VI_dir, STR_dir = STR_dir,
        data_dir = data_dir))
})

test_that("Data-dir directory exists", {
  VI_dir <- system.file("extdata", "NDVI", package = "rOPTRAM")
  STR_dir <- system.file("extdata", "STR", package = "rOPTRAM")
  data_dir <- system.file("extdata", "XXX", package = "rOPTRAM")
  img_date <- "2023-02-19"
  expect_null(optram_calculate_soil_moisture(img_date = img_date,
                                             VI_dir = VI_dir,
                                             STR_dir = STR_dir,
                                             data_dir = data_dir))
})

test_that("Correct image date format", {
  VI_dir <- system.file("extdata", "NDVI", package = "rOPTRAM")
  STR_dir <- system.file("extdata", "STR", package = "rOPTRAM")
  data_dir <- system.file("extdata", package = "rOPTRAM")
  img_date <- "2023-13-16"  # illegal date
  expect_null(optram_calculate_soil_moisture(img_date = img_date,
        VI_dir = VI_dir,
        STR_dir = STR_dir,
        data_dir = data_dir))
})


test_that("Input NDVI directory exists", {
  VI_dir <- system.file("extdata", "XXX", package = "rOPTRAM")
  STR_dir <- system.file("extdata", "STR", package = "rOPTRAM")
  data_dir <- system.file("extdata", package = "rOPTRAM")
  img_date <- "2023-02-19"
  expect_null(optram_calculate_soil_moisture(img_date = img_date,
        VI_dir = VI_dir, STR_dir = STR_dir,
        data_dir = data_dir))
})


test_that("Input STR directory exists", {
  VI_dir <- system.file("extdata", "NDVI", package = "rOPTRAM")
  STR_dir <- system.file("extdata", "XXX", package = "rOPTRAM")
  data_dir <- system.file("extdata", package = "rOPTRAM")
  img_date <- "2023-02-19"
  expect_null(optram_calculate_soil_moisture(img_date = img_date,
        VI_dir = VI_dir, STR_dir = STR_dir,
        data_dir = data_dir))
})

test_that("Length of lists of VI and STR", {
  img_date <- "2023-02-19"
  VI_dir <- system.file("extdata", "NDVI", package = "rOPTRAM")
  data_dir <- system.file("extdata", package = "rOPTRAM")
  # Empty directory, just for testing
  STR_dir <- system.file("extdata", "STR2", package = "rOPTRAM")

  expect_null(optram_calculate_soil_moisture(img_date = img_date,
        VI_dir = VI_dir, STR_dir = STR_dir,
        data_dir = data_dir))

  VI_dir <- system.file("extdata", "NDVI2", package = "rOPTRAM")
  STR_dir <- system.file("extdata", "STR", package = "rOPTRAM")

  expect_null(optram_calculate_soil_moisture(img_date = img_date,
        VI_dir = VI_dir, STR_dir = STR_dir,
        data_dir = data_dir))
})

test_that("Output is a SpatRaster", {
  VI_dir <- system.file("extdata", "NDVI", package = "rOPTRAM")
  STR_dir <- system.file("extdata", "STR", package = "rOPTRAM")
  data_dir <- system.file("extdata",  package = "rOPTRAM")
  img_date <- "2023-02-19"
  W_rast <- optram_calculate_soil_moisture(img_date = img_date,
        VI_dir = VI_dir,
        STR_dir = STR_dir,
        data_dir = data_dir)
  expect_true(inherits(W_rast, "SpatRaster"))
})

test_that("Output is a terra SpatRaster, for exponential trapezoid_method", {
  VI_dir <- system.file("extdata", "NDVI", package = "rOPTRAM")
  STR_dir <- system.file("extdata", "STR", package = "rOPTRAM")
  data_dir <- system.file("extdata", package = "rOPTRAM")
  Output_dir <- tempdir()
  img_date <- "2023-02-19"
  optram_options("trapezoid_method", "exponential")
  W_rast <- optram_calculate_soil_moisture(img_date = img_date,
        VI_dir = VI_dir,
        STR_dir = STR_dir,
        data_dir = data_dir)
  expect_true(inherits(W_rast, "SpatRaster"))
})

