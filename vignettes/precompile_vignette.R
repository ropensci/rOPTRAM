knitr::knit(input = "vignettes/rOPTRAM.Rmd.orig",
            output = "vignettes/rOPTRAM.Rmd")

knitr::knit(input = "vignettes/rOPTRAM_trapezoid_methods.Rmd.orig",
            output = "vignettes/rOPTRAM_trapezoid_methods.Rmd")

# Output directory and plot file name as defined in the vignette:
output_dir <- file.path(tempdir())
figures <- c("trapezoid_Migda_9_linear.png",
             "trapezoid_Migda_9_exponential.png",
             "trapezoid_Migda_9_polynomial.png")
invisible(lapply(figures, function(f) {
  fig_path <- file.path(output_dir, f)
  vig_path <- file.path("vignettes", "images", f)
  file.copy(from = fig_path, to = vig_path)
  }))

devtools::build_vignettes()


