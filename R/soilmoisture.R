#' Calculate soil moisture grid
#'
#' @param img_date, string, image date of Sentinel 2 acquisition
#'
#' @return rast, soil moisture grid
#' @export
#'
#' @examples
#' print("Running soilmoisture.R")

CalculateSoilMoisture <- function(img_date){
  # based on STR and two STR_dry and STR_wet bands
  #  W = (STR - STR_dry) / (STR_wet - STR_dry)
  # W = (i_dry + s_dry * NDVI - STR) / (i_dry − i_wet +  (s_dry−s_wet) * NDVI)

  #
  # Parameters:
  #   coeffs: array of float, dry and wet coefficients
  #   STR_stack: rast of STR rasters
  #   NDVI: rast, the NDVI raster
  #   img_date: string, which date was used
  # Returns:
  #   W: rast, the moisture raster
  img_str <- gsub("-", "", img_date)
  NDVI_file <- list.files(NDVI_dir, pattern=img_str, full.names=TRUE)
  if (! file.exists(NDVI_file)) {
    print(paste("No NDVI file:", NDVI_file, "Exiting..."))
    return(NULL)
  }
  NDVI <- terra::rast(NDVI_file)
  STR_file <- list.files(STR_dir, pattern=img_str, full.names=TRUE)
  STR <- terra::rast(STR_file)

  coeffs <- utils::read.csv(coeffs_file)
  i_dry <- coeffs$intercept_dry
  s_dry <- coeffs$slope_dry
  i_wet <- coeffs$intercept_wet
  s_wet <- coeffs$slope_wet
  W <- (i_dry + s_dry*NDVI - STR) / (i_dry - i_wet +  (s_dry-s_wet)*NDVI)
  outfile <- paste0("soil_moisture_", img_date, ".tif")
  terra::writeRaster(W, file.path(GIS_dir, outfile),
              NAflag=-9999.0, overwrite=TRUE)
  return(W)
}
