test_that("Check that both STR and VI directories contain files", {
  STR_list <- list()
  VI_list <- list.files(system.file("extdata", "SAVI", package = "rOPTRAM"),
                        full.names = TRUE)
  expect_null(optram_ndvi_str(STR_list, VI_list))
  })

test_that("optram_ndvi_str() function returns data.frame", {
  VI_list <- list.files(system.file("extdata", "SAVI", package = "rOPTRAM"),
                        full.names = TRUE)
  STR_list <- list.files(system.file("extdata", "STR", package = "rOPTRAM"),
                        pattern = ".*STR.*tif$",
                        full.names = TRUE)
  expect_true(inherits(optram_ndvi_str(STR_list, VI_list), "data.frame"))
})

test_that("Check that rm.low.vi works", {
  VI_list <- list.files(system.file("extdata", "SAVI", package = "rOPTRAM"),
                        full.names = TRUE)
  STR_list <- list.files(system.file("extdata", "STR", package = "rOPTRAM"),
                        pattern = ".*STR.*tif$",
                        full.names = TRUE)
  full_df <- optram_ndvi_str(STR_list, VI_list, rm.low.vi = TRUE)
  min_ndvi <- min(full_df$VI)
  expect_gte(min_ndvi, 0)
})