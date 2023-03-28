#' @title Create SWIR transformed reflectance
#' @param BOA_dir, string, the path to the `sen2r` Bottom of Atmosphere bands
#' @return list of string, the path to transformed raster
#' @export
#' @examples print("Running optram_calculate_str.R")

optram_calculate_str <- function(BOA_dir){

  # Sadeghi, M., Babaeian, E., Tuller, M., Jones, S.B., 2017.
  # The optical trapezoid model:
  # A novel approach to remote sensing of soil moisture
  # applied to Sentinel-2 and Landsat-8 observations.
  # Remote Sensing of Environment 198, 52–68.
  # https://doi.org/10.1016/j.rse.2017.05.041
  #
  # STR = (1−SWIR)^2 / 2*SWIR
  #
  # Parameters:
  # s2_file: string, full path to multiband geotiff
  #   band 11 is the SWIR
  # Returns:
  #   STR: terra rast object, the SWIR transformed raster

  # Avoid "no visible binding for global variable" NOTE
  BOA_list <- STR_list <- SWIR_DN <- SWIR <- STR <- outfile <- outpath <- NULL
  BOA_list <- list.files(BOA_dir, full.names = TRUE)
  # Get only the original BOA files, in case STR have already been calculated
  BOA_list <- BOA_list[grep("_BOA_", BOA_list)]
  STR_list <- lapply(BOA_list, function(t) {
    stk <- terra::rast(t)
    SWIR_DN <-  stk[[11]]
    # back to native scale
    SWIR <-  SWIR_DN / 10000
    # Convert from Solar irradiance
    # solar_irradiance_12 <- 87.25
    # SWIR <- (SWIR_irr/10) * solar_irradiance_12
    STR <- (1 - SWIR)^2 / (2*SWIR)
    outfile <- gsub("BOA", replacement = "STR", x = basename(t))
    outpath <- file.path(BOA_dir, outfile)
    terra::writeRaster(STR, filename = outpath, overwrite=TRUE)
    return(outpath)
  })
  STR_list <- unlist(STR_list)
  message("Prepared: ", length(STR_list), " STR files")
  # print(STR_list)
  return(STR_list)
}
