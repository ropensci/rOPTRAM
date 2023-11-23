test_that("calculate_vi returns SpatRaster", {

    img_stk <- terra::rast(system.file("extdata",
              "BOA",
              "S2A2A_20230301_121_migdaaoi_BOA_10.tif",
              package = "rOPTRAM"))
    expect_s4_class(calculate_vi(img_stk), "SpatRaster")
})

test_that("calculate_vi returns NULL for non-existant VI", {
    img_stk <- terra::rast(system.file("extdata",
              "BOA",
              "S2A2A_20230301_121_migdaaoi_BOA_10.tif",
              package = "rOPTRAM"))
    expect_null(calculate_vi(img_stk, viname = "XXX"))
})

test_that("calculate_str returns SpatRaster", {

    img_stk <- terra::rast(system.file("extdata",
              "BOA",
              "S2A2A_20230301_121_migdaaoi_BOA_10.tif",
              package = "rOPTRAM"))
    expect_s4_class(calculate_str(img_stk), "SpatRaster")
})

test_that("Check if aoi_file is NULL", {
    expect_false(check_aoi(NULL))
})

test_that("Check if aoi_file is spatial", {
    aoi_file <- system.file("extdata", "s2_config.json",
                    package = "rOPTRAM")
    expect_false(check_aoi(aoi_file))
})

test_that("Check for aoi_name NULL", {
    expect_null(aoi_to_name(NULL))
})

if (package_version(utils::packageVersion("sen2r")) <= '1.5.0') {
    test_that("Version of sen2r is recent", {
        expect_false(check_scihub_access())
    })
} else {
    test_that("sen2r can access scihub", {
        expect_true(check_scihub_access())
})
}

