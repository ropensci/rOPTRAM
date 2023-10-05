#' @title Calculate soil moisture grid
#' @description Prepare soil moisture grid from STR and VI images for a single date
#' using the derived slope and intercept coefficients 
#' @param VI_dir, string, full path to directory holding the VI rasters
#' @param STR_dir, string, full path to directory holding the STR rasters
#' @param img_date, string, image date of single Sentinel 2 acquisition
#'  formatted as "YYYY-MM-DD"
#' @param coeffs_file, string, full path to csv file of wet-dry coefficients
#' @param output_dir, string, full path to output directory 
#'  for saving soil moisture raster
#' @return rast, soil moisture grid
#' @note
#' This function is used after preparing the OPTRAM model coefficients with:
#'  `optram_wetdryt_coefficients()`. Typically a new image date,
#'   that was not used for preparing the model, will be referenced in the `img_date` parameter.
#'   The resulting soil moisture raster is saved to `output_dir`.
#' @export
#' @examples
#' print("Running optram_calculate_soil_moisture.R")

optram_calculate_soil_moisture <- function(
  img_date,
  VI_dir, STR_dir,
  coeffs_file,
  output_dir = tempdir()) {
  # based on STR and two STR_dry and STR_wet bands
  # W = (STR - STR_dry) / (STR_wet - STR_dry)
  # W = (i_dry + s_dry * VI - STR) / (i_dry - i_wet +  (s_dry - s_wet) * VI)
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

  # Pre-flight checks...
  if (is.na(as.Date(img_date, format = "%Y-%m-%d"))) {
    warning("Image date:", img_date, " is not properly formatted",
      "\nPlease format as YYYY-MM-DD")
    return(NULL)
  }
  img_str <- gsub("-", "", img_date)

  if (!dir.exists(VI_dir) || !dir.exists(STR_dir)) {
    warning("Input directories missing, Exiting...")
    return(NULL)
  }

  VI_file <- list.files(VI_dir,
                        pattern = img_str, full.names = TRUE)
  if (length(VI_file) == 0) {
    warning("No NDVI file, Exiting...")
    return(NULL)
  }

  STR_file <- list.files(STR_dir,
                        pattern = paste0(img_str, ".*STR"),
                        full.names = TRUE)
  if (length(STR_file) == 0) {
    warning("No STR file, Exiting...")
    return(NULL)
  }

  if (!file.exists(coeffs_file)) {
    warning("No coefficients file, Exiting...")
    return(NULL)
  }

  # All OK, continue...
  VI <- terra::rast(VI_file[1])
  VI  <- VI / 10000
  STR <- terra::rast(STR_file[1])
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
