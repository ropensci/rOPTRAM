knitr::knit(input = "vignettes/rOPTRAM.Rmd.orig",
            output = "vignettes/rOPTRAM.Rmd")

# Output directory and plot file name as defined in the vignette:
output_dir <- here::here("inst", "extdata", "S2A", "full")
figures <- c("trapezoid_Migda.png")
invisible(lapply(figures, function(f) {
  fig_path <- file.path(output_dir, f)
  file.copy(from = fig_path,
              to = f)
  }))

devtools::build_vignettes()


