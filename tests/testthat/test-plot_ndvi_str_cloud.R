test_that("Check for missing STR and VI columns in full_df", {
  full_df <- readRDS(system.file("extdata", "VI_STR_data.rds", package = "rOPTRAM"))
  full_df1 <- dplyr::select(full_df, !c(STR))
  full_df2 <- dplyr::select(full_df, !c(VI))
  aoi_name <- "Test"
  coeffs <- read.csv(system.file("extdata", "coefficients.csv", package = "rOPTRAM"))
  expect_null(plot_vi_str_cloud(full_df1, coeffs, aoi_name))
  expect_null(plot_vi_str_cloud(full_df2, coeffs, aoi_name))
  })
