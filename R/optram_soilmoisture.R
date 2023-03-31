#' @title Calculate soil moisture grid
#' @description Prepare soil moisture grid from STR and VI images for a single date
#' using the derived slope and intercept coefficients 
#' @param VI_dir, string, full path to directory holding the VI rasters
#' @param STR_dir, string, full path to directory holding the STR rasters
#' @param img_date, string, image date of single Sentinel 2 acquisition
#' @param coeffs_file, string, full path to csv file of wet-dry coefficients
#' @param output_dir, string, full path to output directory 
#'  for saving soil moisture raster
#' @return rast, soil moisture grid
#' @export
#' @examples
#' print("Running optram_calculate_soil_moisture.R")

optram_calculate_soil_moisture <- function(
  img_date,
  VI_dir, STR_dir,
  coeffs_file, output_dir = tempdir()){
  # based on STR and two STR_dry and STR_wet bands
  # W = (STR - STR_dry) / (STR_wet - STR_dry)
  # W = (i_dry + s_dry * VI - STR) / (i_dry − i_wet +  (s_dry−s_wet) * VI)
  #
  # Parameters:
  #   coeffs: array of float, dry and wet coefficients
  #   STR_stack: rast of STR rasters
  #   VI: rast, the NDVI or SAVI raster
  #   img_date: string, which date was used
  # Returns:
  #   W: rast, the moisture raster

  # Avoid "no visible binding for global variable" NOTE
  img_str <- VI_file <-  VI <- NULL
  STR_file <- STR <- coeffs <- NULL
  
  i_dry <- i_wet <- s_dry <- s_wet <- W <- outfile <-  NULL

  img_str <- gsub("-", "", img_date)
  VI_file <- list.files(VI_dir,
                        pattern = img_str, full.names = TRUE)
  if (! file.exists(VI_file)) {
    warning("No NDVI file:", VI_file, "Exiting...")
    return(NULL)
  }
  VI <- terra::rast(VI_file)
  STR_file <- list.files(STR_dir,
                        pattern = paste0(img_str, ".*STR"),
                        full.names = TRUE)
  if (! file.exists(STR_file)) {
    warning("No NDVI file:", VI_file, "Exiting...")
    return(NULL)
  }
  STR <- terra::rast(STR_file)

  if (!file.exists(coeffs_file)) {
    warning("No coefficients file:", coeffs_file, "Exiting...")
    return(NULL)
  }
  coeffs <- utils::read.csv(coeffs_file)
  i_dry <- coeffs$intercept_dry
  s_dry <- coeffs$slope_dry
  i_wet <- coeffs$intercept_wet
  s_wet <- coeffs$slope_wet
  W <- (i_dry + s_dry*VI - STR) / (i_dry - i_wet +  (s_dry-s_wet)*VI)
  outfile <- paste0("soil_moisture_", img_date, ".tif")
  terra::writeRaster(W, file.path(output_dir, outfile),
              NAflag=-9999.0, overwrite=TRUE)
  return(W)
}
