# Resources:
# .onAttach & zzz.R explained: \url{https://r-pkgs.org/r.html#when-you-do-need-side-effects}

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
    optram.period = "full",     # or "seasonal"
    optram.max_cloud = 12,      # max cloud cover [%] between 0 and 100
    optram.vi_step = 0.005,     # values between 0.01 and 0.001
    optram.trapezoid_method = "linear", # "exponential", or "polynomial"
    optram.SWIR_band = 11,      # or 12
    optram.max_tbl_size = 1e+6,  # depends on computer resources
    optram.rm.low.vi    = FALSE,
    optram.rm.hi.str    = FALSE,
    optram.plot_colors = "no",  # "density", "contours", "feature, months"
    optram.feature_col = "ID",  # Which column contains numeric ID's of features
    optram.edge_points = TRUE,   # whether to add edge points to the plot
    optram.only_vi_str = FALSE, # whether to avoid downloading all bands
    optram.tileid = NA, # allow to filter download by Sentinel tile ID
    optram.scm_mask = TRUE, # mask out clouds, cloud shadow, snow, water.
    optram.overwrite = FALSE, # whether to re-download previously acquired images
    optram.save_img_list = FALSE, # whether to save the list of available images
    optram.resolution = 10, # What output resolution (10, 20, or 60)
    optram.area_cover = 99.0 # Only use images that cover at least area-cover percent of aoi
  )

  op <- options()
  toset <- !(names(optram_pkg_opts) %in% names(op))
  if(any(toset)) options(optram_pkg_opts[toset])

  invisible()
}

