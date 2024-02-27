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

test_that("optram_ndvi_str() creates output_dir and returns data.frame", {
  VI_list <- list.files(system.file("extdata", "SAVI", package = "rOPTRAM"),
                        full.names = TRUE)
  STR_list <- list.files(system.file("extdata", "STR", package = "rOPTRAM"),
                         pattern = ".*STR.*tif$",
                         full.names = TRUE)
  output_dir <- file.path(tempdir(), "XXX")
  expect_true(inherits(optram_ndvi_str(STR_list, VI_list,
                                       output_dir = output_dir),
                       "data.frame"))
})

test_that("Check that rm.low.vi and rm.hi.str works", {
  VI_list <- list.files(system.file("extdata", "SAVI", package = "rOPTRAM"),
                        full.names = TRUE)
  STR_list <- list.files(system.file("extdata", "STR", package = "rOPTRAM"),
                        pattern = ".*STR.*tif$",
                        full.names = TRUE)
  full_df <- optram_ndvi_str(STR_list, VI_list, rm.low.vi = TRUE)
  min_ndvi <- min(full_df$VI, na.rm = TRUE)
  expect_gte(min_ndvi, 0)
  full_df <- optram_ndvi_str(STR_list, VI_list, rm.hi.str = TRUE)
  max_str <- max(full_df$STR, na.rm = TRUE)
  expect_lte(max_str, 5)
})
