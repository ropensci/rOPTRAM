skip_if_not_installed("CDSE")

test_that("optram_acquire_s2 returns list", {
  from_date <- "2022-03-01"
  to_date <- "2022-04-30"
  aoi <- sf::st_read(system.file("extdata",
                                 "lachish.gpkg", package = "rOPTRAM"))
  clientid <- Sys.getenv("SCIHUB_CLIENTID")
  if (clientid == "") clientid <- NULL
  secret <- Sys.getenv("SCIHUB_SECRET")
  if (secret == "") secret <- NULL
  boa_list <- optram_acquire_s2(aoi, from_date, to_date,
                                clientid = clientid, secret = secret,
                                save_creds = FALSE)
  expect_true(inherits(boa_list, "list"))
})


