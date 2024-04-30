test_that("Check that plot_vi_str_cloud returns ggplot object", {
  full_df <- readRDS(system.file("extdata", "VI_STR_data.rds",
                                 package = "rOPTRAM"))
  edges_df <- read.csv(system.file("extdata", "trapezoid_edges_lin.csv",
                                         package = "rOPTRAM"))
  pl <- plot_vi_str_cloud(full_df, edges_df)
  expect_true(inherits(pl, "ggplot"))
  })


