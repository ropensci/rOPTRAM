skip_if_not_installed("CDSE")

test_that("Missing AOI file input", {
  from_date <- "2023-03-01"
  to_date  <- "2023-04-30"
  aoi <- NULL
  expect_null(optram_acquire_s2(aoi, from_date, to_date,
                                remote = "scihub", SWIR_band=11))
})

test_that("from_date correctly formatted", {
  # Illegal date
  from_date <- "2023-13-01"
  to_date <- "2023-04-30"
  aoi <- sf::st_read(system.file("extdata",
                                 "lachish.gpkg", package = "rOPTRAM"))
  expect_null(optram_acquire_s2(aoi, from_date, to_date,
                                remote = "scihub", SWIR_band = 11))
  # from_date after to_date
  from_date <- "2023-05-30"
  expect_null(optram_acquire_s2(aoi, from_date, to_date,
                                remote = "scihub", SWIR_band = 11))
})


test_that("Check for invalid clientid and secret to scihub API", {
  from_date <- "2019-04-24"
  to_date <- "2019-04-30"
  aoi <- sf::st_read(system.file("extdata",
                                 "lachish.gpkg", package = "rOPTRAM"))

  expect_null(optram_acquire_s2(aoi, from_date, to_date,
                                remote="scihub",
                                clientid = "xxx", secret = "yyy",
                                save_creds = FALSE, SWIR_band = 11))
})

test_that("Check for invalid SWIR band", {
  from_date <- "2019-04-24"
  to_date <- "2019-04-30"
  aoi <- sf::st_read(system.file("extdata",
                                 "lachish.gpkg", package = "rOPTRAM"))
  expect_null(optram_acquire_s2(aoi, from_date, to_date,
                                remote="scihub",
                                save_creds = FALSE, SWIR_band = 13))
  expect_null(optram_acquire_s2(aoi, from_date, to_date,
                                remote="scihub",
                                save_creds = FALSE, SWIR_band = "B12"))
})
