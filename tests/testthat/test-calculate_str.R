
test_that("Calculate STR returns characters", {
  BOA_dir <- system.file("extdata", "BOA", package = "rOPTRAM")
  STR_dir <- tempdir()
  expect_message(optram_calculate_str(BOA_dir, STR_dir, SWIR_band = 11),
                 "Prepared:")
  expect_type(optram_calculate_str(BOA_dir, STR_dir, SWIR_band = 11),
              "character")
})
