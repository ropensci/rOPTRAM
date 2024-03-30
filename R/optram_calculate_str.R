#' @title Create SWIR Transformed Reflectance
#' @param BOA_dir, string, the path to the Bottom of Atmosphere bands
#' @param STR_dir, string, output directory for STR rasters,
#' @param SWIR_band, integer, band number, either 11 (default) or 12
#' Default is NULL, in which case, the STR_dir will be created alongside BOA_dir
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
#'  SWIR is band 12 (2190 nm) or 11 (1610 nm)
#' @export
#' @examples
#' BOA_dir <- system.file("extdata", "BOA", package = "rOPTRAM")
#' STR_dir = tempdir()
#' STR <- optram_calculate_str(BOA_dir, STR_dir, SWIR_band=11)

optram_calculate_str <- function(BOA_dir,
                                 STR_dir = NULL, SWIR_band = c(11, 12)){
  # Returns: STR_list, list of paths to STR (SWIR Transformed) Raster files
  # Avoid "no visible binding for global variable" NOTE
  BOA_list <- STR_list <- SWIR_DN <- SWIR <- STR <- outfile <- outpath <- NULL

  if (!dir.exists(BOA_dir)) return(NULL)
  if (!check_swir_band(SWIR_band))  return(NULL)
  BOA_list <- list.files(BOA_dir,
                        pattern = ".tif$", full.names = TRUE)
  # Get only the original BOA files, in case STR have already been calculated
  BOA_list <- BOA_list[grep("BOA_", BOA_list)]
  if (length(BOA_list) == 0) return(NULL)
  if (is.null(STR_dir)) {
    STR_dir <- file.path(dirname(BOA_dir), "STR")
  }
  if (!dir.exists(STR_dir)) { dir.create(STR_dir) }

  STR_list <- lapply(BOA_list, function(t) {
    stk <- terra::rast(t)
    # Use band no. 11 = 2200 nm
    SWIR_DN <-  stk[[SWIR_band]]
    # back to native scale
    SWIR <-  SWIR_DN / 10000
    # Convert from Solar irradiance
    # solar_irradiance_12 <- 87.25
    # SWIR <- (SWIR_irr/10) * solar_irradiance_12
    STR <- (1 - SWIR)^2 / (2*SWIR)
    outpath <- file.path(STR_dir,
                         gsub("BOA", replacement = "STR", basename(t)))
    terra::writeRaster(STR, filename = outpath, overwrite=TRUE)
    return(outpath)
  })
  STR_list <- unlist(STR_list)
  message("Prepared: ", length(STR_list), " STR files")
  #print(STR_list)
  return(STR_list)
}
