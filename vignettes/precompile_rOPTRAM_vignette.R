setwd("./vignettes")
knitr::knit(input = file.path("rOPTRAM.Rmd.orig"),
            output = file.path("rOPTRAM.Rmd"))

output_dir <- "/home/micha/EO_Data/Israel/Migda_full"
figures <- c("trapezoid_Migda.png")
invisible(lapply(figures, function(f) {
  fig_path <- file.path(output_dir, f)
  file.copy(from = fig_path,
              to = f)
  file.remove(fig_path)
  }))

devtools::build_vignettes()
setwd("../")
