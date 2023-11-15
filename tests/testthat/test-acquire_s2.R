skip_if_not_installed("sen2r")
skip_on_cran()

test_that("Missing AOI file input", {
  from_date <- "2023-03-01"
  to_date  <- "2023-04-30"
  aoi_file <- NULL
  expect_null(optram_acquire_s2(aoi_file, from_date, to_date))
})

test_that("AOI file is not spatial", {
  from_date <- "2023-03-01"
  to_date  <- "2023-04-30"
  aoi_file <- system.file("extdata", "Migda_9_SM.csv", package = "rOPTRAM")
  expect_null(optram_acquire_s2(aoi_file, from_date, to_date))
})


test_that("API access to scihub not available", {
  from_date <- "2023-03-01"
  to_date <- "2023-04-30"
  aoi_file <- system.file("extdata", "migda_aoi.gpkg", package = "rOPTRAM")
  if (Sys.info()['sysname'] == 'Windows') {
    # Assume that gcloud-sdk is installed in USER's home dir
    # Get the first instance of gsutil file
    homedir <- Sys.getenv("USERPROFILE")
    gsutil_path <- system2("WHERE",
                           paste("/R", homedir, "gsutil"),
                           stdout = TRUE)[1]
  } else {
    gsutil_path <- Sys.which("gsutil")
  }
  gsutil_ok <- sen2r::check_gcloud(gsutil_path, check_creds = FALSE)
  if (!gsutil_ok) {
    expect_null(optram_acquire_s2(aoi_file, from_date, to_date))
  }
})

# test_that("sen2r package is recent",{
#   if (!package_version(utils::packageVersion("sen2r")) > '1.5.0') {
#     from_date <- "2023-03-01"
#     to_date <- "2023-04-30"
#     aoi_file <- system.file("extdata", "migda_aoi.gpkg", package = "rOPTRAM")
#     expect_false(optram_acquire_s2(aoi_file, from_date, to_date))
#   } else {
#     TRUE
#   }
# })
