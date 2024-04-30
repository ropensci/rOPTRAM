
test_that("Calculate STR returns character", {
  BOA_dir <- system.file("extdata", "BOA", package = "rOPTRAM")
  STR_dir <- tempdir()
  expect_message(optram_calculate_str(BOA_dir, STR_dir),
                 "Prepared:")
  expect_type(optram_calculate_str(BOA_dir, STR_dir),
              "character")
})

