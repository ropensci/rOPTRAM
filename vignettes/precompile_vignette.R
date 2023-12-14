knitr::knit(input = "vignettes/rOPTRAM.Rmd.orig",
            output = "vignettes/rOPTRAM.Rmd")

# Output directory and plot file name as defined in the vignette:
output_dir <- file.path(tempdir(), "full")
figures <- c("trapezoid_Migda.png", "sm_map.rds")
invisible(lapply(figures, function(f) {
  fig_path <- file.path(output_dir, f)
  vig_path <- file.path("vignettes", f)
  file.copy(from = fig_path, to = vig_path)
  }))

devtools::build_vignettes()


