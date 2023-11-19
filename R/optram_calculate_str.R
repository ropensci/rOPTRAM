#' @title Create SWIR transformed reflectance
#' @param BOA_dir, string, the path to the `sen2r` Bottom of Atmosphere bands
#' @return list of string, the path to transformed raster
#' @note
#' This function follows:
#' Sadeghi, M., Babaeian, E., Tuller, M., Jones, S.B., 2017.
#'  The optical trapezoid model:
#'  A novel approach to remote sensing of soil moisture
#'  applied to Sentinel-2 and Landsat-8 observations.
#'  Remote Sensing of Environment 198, 52–68.
#'  https://doi.org/10.1016/j.rse.2017.05.041
#'
#' SWIR Transformed Reflectance is calculated as
#'  STR = (1-SWIR)^2 / 2*SWIR
#'
#'  SWIR is band 11 (2200 nm) from `sen2r()` since band B09 is not processed
#'
#' @export
#' @examples 
#' BOA_dir <- system.file("extdata", "BOA")
#' STR <- optram_calculate_str(BOA_dir)

optram_calculate_str <- function(BOA_dir){
  # Returns: STR_list, list of paths to STR (SWIR Transformed) Raster files

  # Avoid "no visible binding for global variable" NOTE
  BOA_list <- STR_list <- SWIR_DN <- SWIR <- STR <- outfile <- outpath <- NULL
  
  if (!dir.exists(BOA_dir)) return(NULL)
  BOA_list <- list.files(BOA_dir,
                        pattern = ".tif$", full.names = TRUE)
  # Get only the original BOA files, in case STR have already been calculated
  BOA_list <- BOA_list[grep("_BOA_", BOA_list)]
  if (length(BOA_list) == 0) return(NULL)
  STR_list <- lapply(BOA_list, function(t) {
    stk <- terra::rast(t)
    # Use band no. 11 = 2200 nm
    SWIR_DN <-  stk[[11]]
    # back to native scale
    SWIR <-  SWIR_DN / 10000
    # Convert from Solar irradiance
    # solar_irradiance_12 <- 87.25
    # SWIR <- (SWIR_irr/10) * solar_irradiance_12
    STR <- (1 - SWIR)^2 / (2*SWIR)
    outpath <- gsub("BOA", replacement = "STR", t)
    outdir <- dirname(outpath)
    if (!dir.exists(outdir)) {
      dir.create(outdir)
    }
    terra::writeRaster(STR, filename = outpath, overwrite=TRUE)
    return(outpath)
  })
  STR_list <- unlist(STR_list)
  message("Prepared: ", length(STR_list), " STR files")
  #print(STR_list)
  return(STR_list)
}
