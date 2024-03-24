#' @title Calculate Soil Moisture Grid
#' @description Prepare soil moisture grid from STR and VI images
#'              for a single date
#' using the derived slope and intercept coefficients
#' @param VI_dir, string, full path to directory holding the VI rasters
#' @param STR_dir, string, full path to directory holding the STR rasters
#' @param img_date, string, image date of single Sentinel 2 acquisition
#'  formatted as "YYYY-MM-DD"
#' @param data_dir, string, the directory where coefficients file was saved
#'  (the `output_dir` parameter in `optram_wetdry_coefficients()` function)
#' @param output_dir, string, full path to output directory
#'  for saving soil moisture raster
#' @return SpatRaster, raster of soil moisture, file(s) saved in output_dir
#' @note
#' This function is used after preparing the OPTRAM model coefficients with:
#'  `optram_wetdry_coefficients()`. Typically a new image date,
#'   (possibly an image that was not used for preparing the model),
#'   will be referenced in the `img_date` parameter.
#'   The resulting soil moisture raster is saved to `output_dir`.
#'
#' Three trapezoid models are offered through the trapezoid_method option:
#'   either "linear", "exponential", or "polynomial". (set using `optram_options()`)
#' The `data_dir` parameter is a directory name.
#' The coefficients CSV file that matches `trapezoid_method`
#' should be in that directory
#'
#'   For further details see:
#' Ambrosone, Mariapaola, et al. 2020.
#' “Retrieving Soil Moisture in Rainfed and Irrigated Fields
#'  Using Sentinel-2 Observations and a Modified OPTRAM Approach.”
#' International Journal of Applied Earth Observation and Geoinformation 89 (July):
#'   102113. https://doi.org/10.1016/j.jag.2020.102113.
#'
#' The data_dir directory must contain the coefficients CSV file
#' i.e. for "linear" method the file was saved as 'coefficients_lin.csv'
#'      for "exponential" it was saved as 'coefficients_exp.csv'
#'      for "polynomial" it was saved as 'coefficients_poly.csv'
#' @export
#' @examples
#' img_date <- "2023-03-11"
#' VI_dir <- system.file("extdata", "SAVI", package = "rOPTRAM")
#' STR_dir <- system.file("extdata", "STR", package = "rOPTRAM")
#' data_dir <- system.file("extdata")
#' SM <- optram_calculate_soil_moisture(img_date,
#'       VI_dir, STR_dir, data_dir)
optram_calculate_soil_moisture <- function(
  img_date,
  VI_dir, STR_dir,
  data_dir,
  output_dir = tempdir()) {

  # Avoid "no visible binding for global variable" NOTE
  VI_files <-  VI <- STR_files <- STR <- coeffs <- NULL
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

  # Possibly two files on same date (because of adjacent tiles)
  VI_files <- list.files(VI_dir,
                        pattern = img_date, full.names = TRUE)
  if (length(VI_files) == 0) {
    message("No VI file, Exiting...")
    return(NULL)
  }

  STR_files <- list.files(STR_dir,
                        pattern = img_date, full.names = TRUE)
  if (length(STR_files) == 0) {
    message("No STR file, Exiting...")
    return(NULL)
  }

  if (!dir.exists(data_dir)) {
    message("No data directory:", data_dir)
    return(NULL)
  }

  trapezoid_method = getOption("optram.trapezoid_method")
  coeffs_file <-  switch(trapezoid_method,
          linear = file.path(data_dir, "coefficients_lin.csv"),
          exponential = file.path(data_dir, "coefficients_exp.csv"),
          polynomial = file.path(data_dir, "coefficients_poly.csv"))
  if (!file.exists(coeffs_file)) {
    message("No coefficients file, Exiting...")
    return(NULL)
  }

  # All OK, continue...
  sm_files <- list()
  for (i in 1:length(VI_files)) {
    f <- VI_files[i]
    # after split: "NDVI", "<date>", "<tileid>"
    date_tile <- unlist(strsplit(gsub(".tif", "", basename(f)), "_"))
    Date <- as.Date(date_tile[2], format="%Y-%m-%d")
    Tile <- date_tile[3]
    VI <- terra::rast(f)
    VI  <- VI / 10000
    STR <- terra::rast(STR_files[i])
    coeffs <- utils::read.csv(coeffs_file)

    W <-  switch(trapezoid_method,
              linear = linear_soil_moisture(coeffs, VI, STR),
              exponential = exponential_soil_moisture(coeffs, VI, STR),
              polynomial = polynomial_soil_moisture(coeffs, VI, STR))
    if (is.null(W)) {
      message("No soil moisture raster created. Exiting...")
      return(NULL)
    }
    outfile <- file.path(output_dir, paste0("soil_moisture_",
                                            Date, "_", Tile, ".tif"))
    terra::writeRaster(W, outfile, NAflag=-9999.0, overwrite=TRUE)
    sm_files[[length(sm_files)+1]] <- outfile
  }
  if (length(sm_files) > 1) {
    message("Multiple tiles created: \n",
            sm_files)
  }
  return(W)
}
