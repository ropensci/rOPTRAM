
test_that("Missing AOI file input", {
  aoi_file <- NULL
  landsat_dir <- system.file(package = "rOPTRAM")
  expect_null(optram_landsat(landsat_dir, aoi_file))
})

test_that("Missing landsat_dir folder", {
  aoi_file <- system.file("extdata", "migda_aoi.gpkg", package = "rOPTRAM")
  landsat_dir <- system.file("extdata", "Landsat", package = "rOPTRAM")  # No such directory
  expect_null(optram_landsat(landsat_dir, aoi_file))
})

test_that("No Landsat images in landsat_dir ", {
  aoi_file <- system.file("extdata", "migda_aoi.gpkg", package = "rOPTRAM")
  landsat_dir <- system.file(package = "rOPTRAM")
  expect_null(optram_landsat(landsat_dir, aoi_file))
})

