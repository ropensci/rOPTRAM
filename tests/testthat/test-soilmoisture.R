
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
  optram_options("trapezoid_method", "exponential", show_opts = FALSE)
  W_rast <- optram_calculate_soil_moisture(img_date = img_date,
        VI_dir = VI_dir,
        STR_dir = STR_dir,
        data_dir = data_dir)
  expect_true(inherits(W_rast, "SpatRaster"))
})

