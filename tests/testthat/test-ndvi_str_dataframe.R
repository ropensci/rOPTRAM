
test_that("optram_ndvi_str() creates output_dir and returns data.frame", {
  VI_list <- list.files(system.file("extdata", "NDVI", package = "rOPTRAM"),
                        full.names = TRUE)
  STR_list <- list.files(system.file("extdata", "STR", package = "rOPTRAM"),
                         full.names = TRUE)
  aoi <- NULL
  # No Feature_ID so only 8 columns
  output_dir <- file.path(tempdir(), "XXX")
  res <- optram_ndvi_str(STR_list, VI_list, aoi=aoi, output_dir = output_dir)
  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 7)
  # With Feature_ID, another column is created,
  # but only if optram_colors = "feature"
  optram_options("plot_colors", "feature")
  aoi <- sf::st_read(system.file("extdata",
                                 "lachish.gpkg", package = "rOPTRAM"))
  res <- optram_ndvi_str(STR_list, VI_list, aoi=aoi, output_dir = output_dir)
  expect_true(inherits(res, "data.frame"))
  expect_equal(ncol(res), 8)
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
  # Get cutoff value at third quartile + 1.5*IQR
  # Leave some extra leeway (1.6*IQR) for IQR calculation
  Q3 <- stats::quantile(full_df$STR, probs = 0.75, na.rm = TRUE)
  STR_IQR <- stats::IQR(full_df$STR, na.rm = TRUE)
  cutoff <- Q3 + 1.6 * STR_IQR
  full_df <- optram_ndvi_str(STR_list, VI_list, output_dir = tempdir(), aoi)
  max_str <- max(full_df$STR, na.rm = TRUE)
  expect_lte(max_str, cutoff)
})

test_that("Check that max_tbl_size works", {
  VI_list <- list.files(system.file("extdata", "NDVI", package = "rOPTRAM"),
                        full.names = TRUE)
  STR_list <- list.files(system.file("extdata", "STR", package = "rOPTRAM"),
                         full.names = TRUE)
  output_dir <- tempdir()
  aoi <- sf::st_read(system.file("extdata",
                                 "lachish.gpkg", package = "rOPTRAM"))
  optram_options("max_tbl_size", 1e4, show_opts = FALSE)
  full_df <- optram_ndvi_str(STR_list, VI_list, output_dir = output_dir, aoi)
  expect_lte(nrow(full_df), getOption("optram.max_tbl_size"))
})
