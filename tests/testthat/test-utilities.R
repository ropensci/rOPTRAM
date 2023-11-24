test_that("calculate_vi returns SpatRaster", {
    img_stk <- terra::rast(system.file("extdata",
              "BOA", "S2A2A_20230301_121_migdaaoi_BOA_10.tif",
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
              "BOA", "S2A2A_20230301_121_migdaaoi_BOA_10.tif",
              package = "rOPTRAM"))
    expect_null(calculate_vi(img_stk, viname = "XXX"))
})

test_that("calculate_str returns SpatRaster", {
    img_stk <- terra::rast(system.file("extdata",
              "BOA", "S2A2A_20230301_121_migdaaoi_BOA_10.tif",
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

if (package_version(utils::packageVersion("sen2r")) <= '1.5.0') {
    test_that("Version of sen2r is recent", {
        expect_false(check_scihub_access())
    })
} else {
    skip_if_offline(host = "r-project.org")
    test_that("sen2r can access scihub", {
        expect_true(check_scihub_access())
})
}

test_that("SCIHUB available", {
    expect_true(check_scihub_access())
     }
)
