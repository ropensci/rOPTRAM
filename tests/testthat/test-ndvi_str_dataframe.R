
test_that("optram_ndvi_str() function returns data.frame", {
  VI_list <- list.files(system.file("extdata", "NDVI", package = "rOPTRAM"),
                        full.names = TRUE)
  STR_list <- list.files(system.file("extdata", "STR", package = "rOPTRAM"),
                        full.names = TRUE)
  aoi <- sf::st_read(system.file("extdata",
                                 "lachish.gpkg", package = "rOPTRAM"))
  expect_true(inherits(optram_ndvi_str(STR_list, VI_list,
                                       output_dir = tempdir(), aoi), "data.frame",))
})

test_that("optram_ndvi_str() creates output_dir and returns data.frame", {
  VI_list <- list.files(system.file("extdata", "NDVI", package = "rOPTRAM"),
                        full.names = TRUE)
  STR_list <- list.files(system.file("extdata", "STR", package = "rOPTRAM"),
                         full.names = TRUE)
  aoi <- sf::st_read(system.file("extdata",
                                 "lachish.gpkg", package = "rOPTRAM"))
  output_dir <- file.path(tempdir(), "XXX")
  expect_true(inherits(optram_ndvi_str(STR_list, VI_list,
                                       output_dir = output_dir, aoi),
                       "data.frame"))
  expect_true(dir.exists(output_dir))
})

test_that("Check that rm.low.vi and rm.hi.str works", {
  VI_list <- list.files(system.file("extdata", "NDVI", package = "rOPTRAM"),
                        full.names = TRUE)
  STR_list <- list.files(system.file("extdata", "STR", package = "rOPTRAM"),
                        full.names = TRUE)
  optram_options("rm.low.vi", TRUE, show_opts = FALSE)
  aoi <- sf::st_read(system.file("extdata",
                                 "lachish.gpkg", package = "rOPTRAM"))
  full_df <- optram_ndvi_str(STR_list, VI_list, output_dir = tempdir(), aoi)
  min_ndvi <- min(full_df$VI, na.rm = TRUE)
  expect_gte(min_ndvi, 0)
  optram_options("rm.hi.str", TRUE, show_opts = FALSE)
  full_df <- optram_ndvi_str(STR_list, VI_list, output_dir = tempdir(), aoi)
  max_str <- max(full_df$STR, na.rm = TRUE)
  expect_lte(max_str, 10)
})

test_that("Check that max_tbl_size works", {
  VI_list <- list.files(system.file("extdata", "NDVI", package = "rOPTRAM"),
                        full.names = TRUE)
  STR_list <- list.files(system.file("extdata", "STR", package = "rOPTRAM"),
                         full.names = TRUE)
  output_dir <- tempdir()
  aoi <- sf::st_read(system.file("extdata",
                                 "lachish.gpkg", package = "rOPTRAM"))
  optram_options("max_tbl_size", 1e3, show_opts = FALSE)
  full_df <- optram_ndvi_str(STR_list, VI_list, output_dir = output_dir, aoi)
  expect_lte(nrow(full_df), getOption("optram.max_tbl_size"))
})
