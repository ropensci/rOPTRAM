test_that("Check prepare_transect() and plot_transect()", {
  Output <- file.path(system.file(package = "rOPTRAM"), "extdata")
  fr <- "2022-10-01"
  to <- "2023-09-30"
  tr_df <- prepare_transect(from_date = fr, to_date = to,
                      output_dir = Output)
  expect_true(inherits(tr_df, "data.frame"))
  expect_equal(ncol(tr_df), 5)

  pl <- plot_transect(tr_df, fr, to, output_dir = Output, save = FALSE)
  expect_true(inherits(pl, "ggplot"))
})

test_that("Check that compile_transect_sizes returns data.frame with 4 columns", {
    Output <- file.path(system.file(package = "rOPTRAM"), "extdata")
    fr <- "2022-10-01"
    to <- "2023-09-30"
    sz_df <- compile_transect_sizes(fr, to, Output)
    expect_true(inherits(sz_df, "data.frame"))
    expect_equal(ncol(sz_df), 4)
    })


