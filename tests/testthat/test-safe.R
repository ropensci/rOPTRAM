<<<<<<< HEAD
test_that("Missing AOI file input", {
  aoi_file <- NULL
  s2_dir <- system.file(package = "rOPTRAM")
  expect_null(optram_safe(s2_dir, aoi_file))
})

=======
>>>>>>> main
test_that("Missing S2_dir folder", {
  aoi <- sf::st_read(system.file("extdata",
                                 "lachish.gpkg", package = "rOPTRAM"))
  S2_dir <- system.file("extdata", "S2", package = "rOPTRAM")  # No such directory
<<<<<<< HEAD
  expect_null(optram_safe(S2_dir, aoi_file))
=======
  expect_null(optram_safe(S2_dir, aoi, SWIR_band = 11))
>>>>>>> main
})


test_that("No S2 images in safe_dir ", {
  aoi <- sf::st_read(system.file("extdata",
                                 "lachish.gpkg", package = "rOPTRAM"))
  # Wrong directory, no SAFE directories here:
  S2_dir <- system.file("extdata", "BOA", package = "rOPTRAM")
<<<<<<< HEAD
  expect_null(optram_safe(S2_dir, aoi_file))
=======
  expect_null(optram_safe(S2_dir, aoi, SWIR_band = 11))
>>>>>>> main
})
