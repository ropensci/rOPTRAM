test_that("Check plot_transect() returns ggplot object", {
  Output <- file.path(system.file(package = "rOPTRAM"), "extdata")
  fr <- "2022-10-01"
  to <- "2023-09-30"
  Output <- file.path(system.file(package = "rOPTRAM"), "extdata")
  pl <- plot_transect(from_date = fr, to_date = to,
                      output_dir = Output, save = FALSE)
  expect_true(inherits(pl, "ggplot"))
})

