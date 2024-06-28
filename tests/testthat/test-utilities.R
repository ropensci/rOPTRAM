test_that("Check that calculate_vi returns SpatRaster", {
    img_stk <- terra::rast(system.file("extdata",
              "BOA", "BOA_2022-12-11.tif", package = "rOPTRAM"))
    # Test all VIs
    optram_options("veg_index", "SAVI", show_opts = FALSE)
    expect_s4_class(calculate_vi(img_stk), "SpatRaster")
    optram_options("veg_index", "NDVI", show_opts = FALSE)
    expect_s4_class(calculate_vi(img_stk), "SpatRaster")
    optram_options("veg_index", "MSAVI", show_opts = FALSE)
    expect_s4_class(calculate_vi(img_stk), "SpatRaster")
})

test_that("Check that calculate_str returns SpatRaster", {
    img_stk <- terra::rast(system.file("extdata",
              "BOA", "BOA_2022-12-11.tif", package = "rOPTRAM"))
    expect_s4_class(calculate_str(img_stk), "SpatRaster")
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
                                             "Library", "Preferences", ".CDSE")})
  testthat::skip_if_not(dir.exists(creds_path))
  creds <- retrieve_cdse_credentials()
  expect_true(inherits(creds, "list"))
  expect_equal(length(creds[[1]]), 2)
})

test_that("CDSE credentials can be stored fronm environment variables", {
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

test_that("Check for valid optram options", {
  expect_null(optram_options())
  expect_message(optram_options("veg_index", "XXX", show_opts = FALSE),
                 "Incorrect value:")
  expect_message(optram_options("trapezoid_method", "EXP", show_opts = FALSE),
                 "Incorrect value:")
  expect_message(optram_options("period", "season", show_opts = FALSE),
                 "Incorrect value:")
  expect_message(optram_options("vi_step", "0.01", show_opts = FALSE),
                 "Incorrect value:")
  expect_message(optram_options("SWIR_band", 13, show_opts = FALSE),
                 "Incorrect value:")
  expect_message(optram_options("SWIR_band", "B11", show_opts = FALSE),
                 "Incorrect value:")
  expect_message(optram_options("max_tbl_size", 1e2, show_opts = FALSE),
                 "Incorrect value:")
  expect_message(optram_options("plot_colors", TRUE, show_opts = FALSE),
                 "Incorrect value:")
  expect_message(optram_options("remote", "CDSE", show_opts = FALSE),
                 "Incorrect value:")
  expect_message(optram_options("use_scihub", TRUE, show_opts = FALSE),
                 "Unknown option name:")
})

