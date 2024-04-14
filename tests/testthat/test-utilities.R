test_that("Check that calculate_vi returns SpatRaster", {
    img_stk <- terra::rast(system.file("extdata",
              "BOA", "BOA_2022-12-11.tif", package = "rOPTRAM"))
    # Test all VIs
    expect_s4_class(calculate_vi(img_stk, viname = "NDVI"), "SpatRaster")
    expect_s4_class(calculate_vi(img_stk, viname = "SAVI"), "SpatRaster")
    expect_s4_class(calculate_vi(img_stk, viname = "MSAVI"), "SpatRaster")
    expect_s4_class(calculate_vi(img_stk, viname = "CI"), "SpatRaster")
    expect_s4_class(calculate_vi(img_stk, viname = "BSCI"), "SpatRaster")
})

test_that("Check that calculate_vi returns NULL for non-existant VI", {
    img_stk <- terra::rast(system.file("extdata",
              "BOA", "BOA_2022-12-11.tif", package = "rOPTRAM"))
    expect_null(calculate_vi(img_stk, viname = "XXX"))
})

test_that("In calculate_vi, img_stk has all 12 bands", {
  img_stk <- terra::rast(system.file("extdata",
                                     "BOA", "BOA_2022-12-11.tif", package = "rOPTRAM"))
  short_stk <- img_stk[[1:10]]
  expect_null(calculate_vi(short_stk, viname = "NDVI"))
})

test_that("Check that calculate_str returns SpatRaster", {
    img_stk <- terra::rast(system.file("extdata",
              "BOA", "BOA_2022-12-11.tif", package = "rOPTRAM"))
    expect_s4_class(calculate_str(img_stk, SWIR_band = 11), "SpatRaster")
})

test_that("Check for invalid SWIR_band value", {
  img_stk <- terra::rast(system.file("extdata",
                                     "BOA", "BOA_2022-12-11.tif",
                                     package = "rOPTRAM"))
  expect_null(calculate_str(img_stk, SWIR_band = 10))
  short_stk <- img_stk[[1:10]]
  expect_null(calculate_str(short_stk, SWIR_band=11))
})

test_that("Check if aoi_file is NULL", {
    expect_false(check_aoi(aoi_file = ""))
})

test_that("Check for invalid SWIR band", {
  expect_false(check_swir_band(13))
  expect_false(check_swir_band("B12"))
})
test_that("Check if aoi_file is spatial", {
    # Non spatial file
    aoi_file <- system.file("extdata", "coefficients_lin.csv",
                    package = "rOPTRAM")
    expect_false(check_aoi(aoi_file))
    # Non existing file
    expect_false(check_aoi("blabla.txt"))
    aoi_file <- system.file("extdata", "some_area.gpkg",
                            package = "rOPTRAM")
    expect_false(check_aoi(aoi_file))
})

test_that("Check for aoi_name NULL", {
    expect_null(aoi_to_name(NULL))
})

test_that("Check aoi_to_name return value",{
    # Test conversion of file name to aoi_name string
    aoi_file <- system.file("extdata", "some_area.gpkg", package = "rOPTRAM")
    expect_equal(aoi_to_name(aoi_file), "somearea")
})

test_that("Check format of from_date, to_date",{
  expect_false(check_date_string(2023-01-01, "2023-02-02"))
  expect_false(check_date_string("2023/01/01", "2023-02-02"))
  expect_false(check_date_string("2023-01-01", "23-02-01"))
  expect_false(check_date_string("30-01-2023", "2023-02-02"))
  expect_false(check_date_string("2022-13-01", "2023-02-01"))
  expect_false(check_date_string("2023-10-03", "2023-10-32"))
  expect_false(check_date_string("2023-11-03", "2023-11-01"))
  expect_true(check_date_string("2023-10-31", "2023-11-30"))
})

test_that("CDSE credentials are retrieved", {
  creds_path <- switch(Sys.info()['sysname'],
                  "Windows" = {file.path(Sys.getenv("LOCALAPPDATA"), "CDSE")},
                  "Linux" = {file.path(Sys.getenv("HOME"), ".CDSE")},
                  "Darwin" = {file.path(Sys.getenv("HOME"),
                                      "Library", "Preferences", ".CDSE")}
  )
  testthat::skip_if_not(dir.exists(creds_path))
  creds <- retrieve_cdse_credentials()
  expect_true(inherits(creds, "list"))
  expect_equal(length(creds[[1]]), 2)
})

test_that("CDSE credentials can be stored from environment variables", {
  # First get current creds, if they are already stored
  creds <- retrieve_cdse_credentials()
  # Set environment variables for this test from already stored creds
  if (!is.null(creds)) {
    # Get current creds into env variables
    Sys.setenv(OAUTH_CLIENTID = creds[[1]]$clientid)
    Sys.setenv(OAUTH_SECRET = creds[[1]]$secret)
  }
  # Are env variables available?
  skip_if(nchar(Sys.getenv("OAUTH_CLIENTID")) < 39 |
          nchar(Sys.getenv("OAUTH_SECRET")) < 32)
  expect_message(store_cdse_credentials(NULL, NULL), "Credentials")
  }
)
