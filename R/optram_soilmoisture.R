#' @title Calculate Soil Moisture Grid
#' @description Prepare soil moisture grid from STR and VI images
#'              for a single date
#' using the derived slope and intercept coefficients
#' @param VI_dir, string, full path to directory holding the VI rasters
#' @param STR_dir, string, full path to directory holding the STR rasters
#' @param img_date, string, image date of single Sentinel 2 acquisition
#'  formatted as "YYYY-MM-DD"
#' @param data_dir, string, the directory where coefficients file was saved
#'  (the \code{output_dir} parameter in \code{\link[rOPTRAM]{optram_wetdry_coefficients}} function)
#' @param output_dir, string, full path to output directory
#'  for saving soil moisture raster
#' @param trapezoid_method, string,
#'  either "linear", "exponential", or "polynomial", default is "linear"
#' @return rast, soil moisture grid
#' @note
#' This function is used after preparing the OPTRAM model coefficients with:
#'  \code{\link[rOPTRAM]{optram_wetdry_coefficients}}. Typically a new image date,
#'   (possibly an image that was not used for preparing the model),
#'   will be referenced in the \code{img_date} parameter.
#'   The resulting soil moisture raster is saved to \code{output_dir}.
#'
#' Three trapezoid models are offered through the trapezoid_method parameter:
#'   either "linear", "exponential", or "polynomial".
#' The \code{data_dir} parameter is a directory name.
#' The coefficients CSV file that matches \code{trapezoid_method}
#' should be in that directory
#'
#'   For further details see:
#' Ambrosone, Mariapaola, et al. 2020.
#' “Retrieving Soil Moisture in Rainfed and Irrigated Fields
#'  Using Sentinel-2 Observations and a Modified OPTRAM Approach.”
#' International Journal of Applied Earth Observation and Geoinformation 89 (July):
#'   102113. \doi{https://doi.org/10.1016/j.jag.2020.102113}.
#'
#' The data_dir directory must contain the coefficients CSV file
#' i.e. for "linear" method the file was saved as 'coefficients_lin.csv'
#'      for "exponential" it was saved as 'coefficients_exp.csv'
#'      for "polynomial" it was saved as 'coefficients_poly.csv'
#' @export
#' @examples
#' img_date <- "2023-03-11"
#' VI_dir <- system.file("extdata", "NDVI", package = "rOPTRAM")
#' STR_dir <- system.file("extdata", "STR", package = "rOPTRAM")
#' data_dir <- system.file("extdata")
#' SM <- optram_calculate_soil_moisture(img_date,
#'       VI_dir, STR_dir,
#'       data_dir,
#'       trapezoid_method = "linear")
optram_calculate_soil_moisture <- function(
  img_date,
  VI_dir, STR_dir,
  data_dir,
  output_dir = tempdir(),
  trapezoid_method = c("linear", "exponential", "polynomial")) {

  # Avoid "no visible binding for global variable" NOTE
  VI_file <-  VI <- STR_file <- STR <- coeffs <- NULL
  i_dry <- i_wet <- s_dry <- s_wet <- W <- outfile <-  NULL

  # Pre-flight checks...
  if (is.na(as.Date(img_date, format = "%Y-%m-%d"))) {
    message("Image date:", img_date, " is not properly formatted",
      "\nPlease format as YYYY-MM-DD")
    return(NULL)
  }

  if (!dir.exists(VI_dir) || !dir.exists(STR_dir)) {
    message("Input directories missing, Exiting...")
    return(NULL)
  }

  VI_file <- list.files(VI_dir,
                        pattern = img_date, full.names = TRUE)
  if (length(VI_file) == 0) {
    message("No VI file, Exiting...")
    return(NULL)
  }

  STR_file <- list.files(STR_dir,
                        pattern = img_date, full.names = TRUE)
  if (length(STR_file) == 0) {
    message("No STR file, Exiting...")
    return(NULL)
  }

  if (!dir.exists(data_dir)) {
    message("No data directory:", data_dir)
    return(NULL)
  }

  coeffs_file <-  switch(trapezoid_method,
          linear = file.path(data_dir, "coefficients_lin.csv"),
          exponential = file.path(data_dir, "coefficients_exp.csv"),
          polynomial = file.path(data_dir, "coefficients_poly.csv"))
  if (!file.exists(coeffs_file)) {
    message("No coefficients file, Exiting...")
    return(NULL)
  }

  # All OK, continue...
  VI <- terra::rast(VI_file[1])
  VI  <- VI / 10000
  STR <- terra::rast(STR_file[1])
  coeffs <- utils::read.csv(coeffs_file)

  tryCatch(
    expr = {trapezoid_method <- match.arg(trapezoid_method)},
    error = function(e) { return(NULL) })
  W <-  switch(trapezoid_method,
            linear = linear_soil_moisture(coeffs, VI, STR),
            exponential = exponential_soil_moisture(coeffs, VI, STR),
            polynomial = polynomial_soil_moisture(coeffs, VI, STR))
  if (is.null(W)) {
    message("No soil moisture raster created. Exiting...")
    return(NULL)
  }
  outfile <- file.path(output_dir, paste0("soil_moisture_", img_date, ".tif"))
  terra::writeRaster(W, outfile, NAflag=-9999.0, overwrite=TRUE)
  return(W)
}
