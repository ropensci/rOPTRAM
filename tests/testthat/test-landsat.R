test_that("Missing landsat_dir folder", {
  aoi <- sf::st_read(system.file("extdata",
                                 "lachish.gpkg", package = "rOPTRAM"))
  landsat_dir <- system.file("extdata", "Landsat", package = "rOPTRAM")  # No such directory
  expect_null(optram_landsat(landsat_dir, aoi))
})

test_that("No Landsat images in landsat_dir ", {
  aoi <- sf::st_read(system.file("extdata",
                                 "lachish.gpkg", package = "rOPTRAM"))
  landsat_dir <- system.file(package = "rOPTRAM")
  expect_null(optram_landsat(landsat_dir, aoi))
})

