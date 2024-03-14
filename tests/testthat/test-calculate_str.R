test_that("Wrong BOA directory", {
  # Non-existing directory
  BOA_dir <- "XXX"
  STR_dir <- tempdir()
  expect_null(optram_calculate_str(BOA_dir, STR_dir = STR_dir, SWIR_band = 11))
  # Wrong directory, no BOA files
  BOA_dir <- system.file("extdata", "NDVI", package = "rOPTRAM")
  expect_null(optram_calculate_str(BOA_dir, STR_dir, SWIR_band = 11))
})

test_that("BOA directory has files", {
  # Empty directory for testing
  BOA_dir <- system.file("extdata", "BOA2", package = "rOPTRAM")
  STR_dir <- tempdir()
  expect_null(optram_calculate_str(BOA_dir, STR_dir, SWIR_band = 11))
})

test_that("Calculate STR", {
  BOA_dir <- system.file("extdata", "BOA", package = "rOPTRAM")
  STR_dir <- tempdir()
  expect_message(optram_calculate_str(BOA_dir, STR_dir, SWIR_band = 11),
                 "Prepared:")
  expect_type(optram_calculate_str(BOA_dir, STR_dir, SWIR_band = 11),
              "character")
})

test_that("Check for invalid SWIR band", {
  BOA_dir <- system.file("extdata", "BOA", package = "rOPTRAM")
  STR_dir <- tempdir()
  expect_null(optram_calculate_str(BOA_dir, STR_dir, SWIR_band = 13))
  expect_null(optram_calculate_str(BOA_dir, STR_dir, SWIR_band = "B12"))
})
