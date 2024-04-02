# Resources:
# .onAttach & zzz.R explained: https://r-pkgs.org/r.html#when-you-do-need-side-effects

.onAttach <- function(libname, pkgname) {
  m <- c(sprintf("rOPTRAM version %s",
                 utils::packageVersion("rOPTRAM")),
         "Use optram_options() to display and set package options")
  packageStartupMessage(paste(m, collapse = "\n"))
}

.onLoad <- function(libname, pkgname) {
  optram_pkg_opts <- list(
    optram.veg_index = "NDVI",  # or "SAVI"
    optram.remote = "scihub",   # or "openeo
    optram.vi_step = 0.005,     # values between 0.01 and 0.001
    optram.trapezoid_method = "linear", # "exponential", or "polynomial"
    optram.SWIR_band = 11,      # or 12
    optram.max_tbl_size = 1e+6,  # depends on computer resources
    optram.rm.low.vi    = FALSE,
    optram.rm.hi.str    = FALSE,
    optram.plot_density = "no",  # "colors"
    optram.edge_points = TRUE    # whether to add edge points to the plot
  )

  op <- options()
  toset <- !(names(optram_pkg_opts) %in% names(op))
  if(any(toset)) options(optram_pkg_opts[toset])

  invisible()
}
