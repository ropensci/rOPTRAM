skip_if_not_installed("CDSE")

test_that("optram_acquire_s2 returns list", {
  from_date <- "2023-03-01"
  to_date <- "2023-04-30"
  aoi <- sf::st_read(system.file("extdata",
                                 "lachish.gpkg", package = "rOPTRAM"))

  boa_list <- optram_acquire_s2(aoi, from_date, to_date,
                                remote = "scihub", SWIR_band = 11)
  expect_true(inherits(boa_list, "list"))
})

