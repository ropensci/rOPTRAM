skip_if_not_installed("sen2r")

test_that("Missing AOI file input", {
  from_date <- "2023-03-01"
  to_date  <- "2023-04-30"
  aoi_file <- NULL
  expect_null(optram_acquire_s2(aoi_file, from_date, to_date,
                                remote = "gcloud"))
})

test_that("AOI file is not spatial", {
  from_date <- "2023-03-01"
  to_date  <- "2023-04-30"
  aoi_file <- system.file("extdata", "Migda_9_SM.csv", package = "rOPTRAM")
  expect_null(optram_acquire_s2(aoi_file, from_date, to_date))
  from_date <- "2023-03-01"
  to_date <- "2023-04-32"
  expect_null(optram_acquire_s2(aoi_file, from_date, to_date,
                                remote = "gcloud"))
})

test_that("from_date correctly formatted", {
  # Illegal date
  from_date <- "2023-13-01"
  to_date <- "2023-04-30"
  aoi_file <- system.file("extdata", "Migda_9_SM.csv", package = "rOPTRAM")
  expect_null(optram_acquire_s2(aoi_file, from_date, to_date,
                                remote = "gcloud"))
  # from_date after to_date
  from_date <- "2023-05-30"
  expect_null(optram_acquire_s2(aoi_file, from_date, to_date,
                                remote = "gcloud"))
})

test_that("API access to scihub ", {
  # Short time interval to get only one downloaded file
  from_date <- "2019-04-24"
  to_date <- "2019-04-30"
  aoi_file <- system.file("extdata", "migda.gpkg", package = "rOPTRAM")
  gsutil_path <- NULL
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
  ifelse(is.null(gsutil_path) |
          gsutil_path == "" |
          is.na(gsutil_path) |
          !sen2r::is_gcloud_configured(),
    gcloud_ok <- FALSE,
    gcloud_ok <- sen2r::check_gcloud(gsutil_path, check_creds = FALSE))

  if (!gcloud_ok) {
    expect_null(optram_acquire_s2(aoi_file, from_date, to_date,
                                  remote = "gcloud"))
  } else if (sen2r::is_gcloud_configured()) {
    result_list <- optram_acquire_s2(aoi_file, from_date, to_date,
                                     remote = "gcloud")
    expect_type(result_list, "character")
    expect_length(result_list, 2)
  }
})

test_that("Access to scihub API and openEO platform", {
  # TODO: this is a stub,
  # to be revised when acquire_openeo() and acquire_scihub() are written
  from_date <- "2019-04-24"
  to_date <- "2019-04-30"
  aoi_file <- system.file("extdata", "migda.gpkg", package = "rOPTRAM")
  expect_null(optram_acquire_s2(aoi_file, from_date, to_date, remote="openeo"))
  expect_null(optram_acquire_s2(aoi_file, from_date, to_date, remote="scihub"))
})
