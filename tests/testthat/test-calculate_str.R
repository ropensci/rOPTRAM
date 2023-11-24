test_that("Wrong BOA directory", {
  # Non-existing directory
  BOA_dir <- "XXX"
  expect_null(optram_calculate_str(BOA_dir))
  # Wrong directory, no BOA files
  BOA_dir <- system.file("extdata", "NDVI", package = "rOPTRAM")
  expect_null(optram_calculate_str(BOA_dir))
})

test_that("BOA directory has files", {
  # Empty directory for testing
  BOA_dir <- system.file("extdata", "BOA2", package = "rOPTRAM")
  expect_null(optram_calculate_str(BOA_dir))
})

test_that("Calculate STR", {
  BOA_dir <- system.file("extdata", "BOA", package = "rOPTRAM")
  expect_message(optram_calculate_str(BOA_dir), "Prepared:")
  expect_type(optram_calculate_str(BOA_dir), "character")
})
