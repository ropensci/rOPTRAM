test_that("calculate_vi returns SpatRaster", {
    img_stk <- terra::rast(system.file("extdata",
              "BOA", "S2A2A_20230301_121_migda_BOA_10.tif",
              package = "rOPTRAM"))
    # Test all VIs
    expect_s4_class(calculate_vi(img_stk, viname = "NDVI"), "SpatRaster")
    expect_s4_class(calculate_vi(img_stk, viname = "SAVI"), "SpatRaster")
    expect_s4_class(calculate_vi(img_stk, viname = "MSAVI"), "SpatRaster")
    expect_s4_class(calculate_vi(img_stk, viname = "CI"), "SpatRaster")
    expect_s4_class(calculate_vi(img_stk, viname = "BSCI"), "SpatRaster")
})

test_that("calculate_vi returns NULL for non-existant VI", {
    img_stk <- terra::rast(system.file("extdata",
              "BOA", "S2A2A_20230301_121_migda_BOA_10.tif",
              package = "rOPTRAM"))
    expect_null(calculate_vi(img_stk, viname = "XXX"))
})

test_that("calculate_str returns SpatRaster", {
    img_stk <- terra::rast(system.file("extdata",
              "BOA", "S2A2A_20230301_121_migda_BOA_10.tif",
              package = "rOPTRAM"))
    expect_s4_class(calculate_str(img_stk), "SpatRaster")
})

test_that("Check if aoi_file is NULL", {
    expect_false(check_aoi(aoi_file = ""))
})

test_that("Check if aoi_file is spatial", {
    # Non spatial file
    aoi_file <- system.file("extdata", "s2_config.json",
                    package = "rOPTRAM")
    expect_false(check_aoi(aoi_file))
    # Non existing file
    expect_false(check_aoi("blabla.txt"))
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

skip_if_not_installed("sen2r")
site <- "http://scihub.copernicus.eu"
if (package_version(utils::packageVersion("sen2r")) <= '1.5.0') {
    test_that("Version of sen2r is recent", {
        expect_false(check_scihub_access(site))
    })
} else {
    test_that("sen2r can access scihub", {
        expect_true(check_scihub_access(site))
    })
}

