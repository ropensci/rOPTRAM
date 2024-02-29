knitr::knit(input = "vignettes/rOPTRAM.Rmd.orig",
            output = "vignettes/rOPTRAM.Rmd")

knitr::knit(input = "vignettes/rOPTRAM_trapezoid_methods.Rmd.orig",
            output = "vignettes/rOPTRAM_trapezoid_methods.Rmd")

# Output directory and plot file name as defined in the vignette:
output_dir <- tempdir()
figures <- c("trapezoid_lachish_linear.png",
             "trapezoid_lachish_exponential.png",
             "trapezoid_lachish_polynomial.png")
invisible(lapply(figures, function(f) {
  fig_path <- file.path(output_dir, f)
  vig_path <- file.path("vignettes", "images", f)
  #file.copy(from = fig_path, to = vig_path)
  im_obj <- magick::image_read(fig_path)
  res_obj <- magick::image_resize(im_obj, "50%")
  magick::image_write(res_obj, vig_path)
  }))

devtools::build_vignettes()


