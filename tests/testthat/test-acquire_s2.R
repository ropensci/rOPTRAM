skip_if_not_installed("CDSE")

test_that("Missing AOI file input", {
  from_date <- "2023-03-01"
  to_date  <- "2023-04-30"
  aoi_file <- NULL
  expect_null(optram_acquire_s2(aoi_file, from_date, to_date,
                                remote = "scihub"))
})

test_that("AOI file is not spatial", {
  from_date <- "2023-03-01"
  to_date  <- "2023-04-30"
  aoi_file <- system.file("extdata", "Migda9_VWC.csv", package = "rOPTRAM")
  expect_null(optram_acquire_s2(aoi_file, from_date, to_date))
  from_date <- "2023-03-01"
  to_date <- "2023-04-32"
  expect_null(optram_acquire_s2(aoi_file, from_date, to_date,
                                remote = "xxx"))
})

test_that("from_date correctly formatted", {
  # Illegal date
  from_date <- "2023-13-01"
  to_date <- "2023-04-30"
  aoi_file <- system.file("extdata", "migda_9.gpkg", package = "rOPTRAM")
  expect_null(optram_acquire_s2(aoi_file, from_date, to_date,
                                remote = "scihub"))
  # from_date after to_date
  from_date <- "2023-05-30"
  expect_null(optram_acquire_s2(aoi_file, from_date, to_date,
                                remote = "scihub"))
})


test_that("Check for invalid clientid and secret to scihub API", {
  from_date <- "2019-04-24"
  to_date <- "2019-04-30"
  aoi_file <- system.file("extdata", "migda_9.gpkg", package = "rOPTRAM")

  expect_null(optram_acquire_s2(aoi_file, from_date, to_date,
                                remote="scihub",
                                clientid = "xxx",
                                secret = "yyy",
                                save_creds = FALSE))
})
