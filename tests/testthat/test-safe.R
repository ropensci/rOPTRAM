test_that("Missing AOI file input", {
  aoi_file <- NULL
  safe_dir <- system.file(package = "rOPTRAM")
  expect_null(optram_safe(safe_dir, aoi_file))
})

test_that("Missing safe_dir folder", {
  aoi_file <- system.file("extdata", "migda_aoi.gpkg", package = "rOPTRAM")
  safe_dir <- NULL
  expect_null(optram_safe(safe_dir, aoi_file))
})

test_that("No SAFE formatted images in safe_dir ", {
  aoi_file <- system.file("extdata", "migda_aoi.gpkg", package = "rOPTRAM")
  safe_dir <- system.file(package = "rOPTRAM")
  expect_null(optram_safe(safe_dir, aoi_file))
})

