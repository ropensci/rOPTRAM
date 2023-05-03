test_that("Check STR and VI lists", {
  STR_list <- list()
  VI_list <- list.files(system.file("extdata", "NDVI", package = "rOPTRAM"),
                        full.names = TRUE)
  expect_warning(optram_ndvi_str(STR_list, VI_list), "No raster files in directories")
  STR_list <- list.files(system.file("extdata", "BOA", package = "rOPTRAM"),
                        pattern = ".*STR.*tif$",
                        full.names = TRUE)
  expect_true(inherits(optram_ndvi_str(STR_list, VI_list), "data.frame"))
})
