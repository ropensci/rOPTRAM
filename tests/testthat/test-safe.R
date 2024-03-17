test_that("Missing AOI file input", {
  aoi_file <- NULL
  s2_dir <- system.file(package = "rOPTRAM")
  expect_null(optram_safe(s2_dir, aoi_file))
})

test_that("Missing S2_dir folder", {
  aoi_file <- system.file("extdata", "lachish.gpkg", package = "rOPTRAM")
  S2_dir <- system.file("extdata", "S2", package = "rOPTRAM")  # No such directory
  expect_null(optram_safe(S2_dir, aoi_file))
})


test_that("No S2 images in safe_dir ", {
  aoi_file <- system.file("extdata", "lachish.gpkg", package = "rOPTRAM")
  # Wrong directory, no SAFE directories here:
  S2_dir <- system.file("extdata", "BOA", package = "rOPTRAM")
  expect_null(optram_safe(S2_dir, aoi_file))
})
