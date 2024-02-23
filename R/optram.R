#' @title Prepare Sentinel Imagery for Soil Moisture Prediction Using OPTRAM.
#' @description
#' The main wrapper function to download, and preprocess Sentinel 2 imagery
#' then prepare the OPTRAM trapezoid to derive slope and intercept
#' for wet and dry trapezoid lines.
#' See:
#'   Sadeghi, M., Babaeian, E., Tuller, M., Jones, S.B., 2017.
#'   The optical trapezoid model:
#'   A novel approach to remote sensing of soil moisture
#'   applied to Sentinel-2 and Landsat-8 observations.
#'   Remote Sensing of Environment 198, 52–68,
#'   https://doi.org/10.1016/j.rse.2017.05.041 .
#' @param aoi_file, string, full path to polygon spatial file
#'        of area of interest
#' @param veg_index, string, which index to use. Default "NDVI"
#' @param from_date, string, the start of the date range,
#'        Formatted as "YYYY-MM-DD"
#' @param to_date, the end of the date range.
#' @param max_cloud, integer, maximum percent cloud cover, Default 15.
#' @param S2_output_dir, string, directory to save downloaded S2
#'  and the derived products, defaults to tempdir()
#' @param data_output_dir, string, path to save coeffs_file
#'  and STR-VI data.frame, default is tempdir()
#' @param step, float, width of intervals along VI axis
#'  default 0.01
#' @param remote, string, which Copernicus API to use,
#'  one of "scihub", or "openeo". Default is "scihub"
#' @param trapezoid_method, string,
#'  one of "linear", "exponential", "polynomial"
#'  default "linear"
#'  How to fit a curve to the values along trapezoid edges
#'  See notes in optram_wetdry_coefficients()
#'
#' @return rmse_list, list, RMSE values of fitted trapezoid lines
#' the coefficients are also saved to a csv file in `data_output_dir`.
#' @note
#' to download imagery. Please first install `gcloud` following instructions:
#' https://cloud.google.com/sdk/docs/install
#' for your operating system.
#' And be sure to initialize with you google username and password.
#'
#' Output can be separated:
#' Sentinel downloads and products are saved to S2_output_dir.
#' Data files (Trapezoid coefficients and STR-VI data) to data_output_dir
#'
#' @export
#' @examples
#' \dontrun{
#' from_date <- "2018-12-01"
#' to_date <- "2020-04-30"
#' aoi_file <- system.file("extdata", "migda.gpkg", package = "rOPTRAM")
#' coeffs <- optram(aoi_file,
#'                  from_date, to_date,
#'                  veg_index = c("SAVI"),
#'                  trapezoid_method = "linear")
#' }


optram <- function(aoi_file,
                   veg_index = 'NDVI',
                   from_date, to_date,
                   max_cloud = 15,
                   remove_safe = "yes",
                   S2_output_dir = tempdir(),
                   data_output_dir = tempdir(),
                   remote = "scihub",
                   step = 0.01,
                   trapezoid_method = c("linear", "exponential", "polynomial")) {

  # Avoid "no visible binding for global variable" NOTE
  access_ok <- s2_list <- s2_dirs <- BOA_dir <- NULL
  VI_dir <- VI_list <- VI_STR_df <- coeffs  <- NULL

    # Loop over the downloaded S2 folders (dates),
    # create NDVI and STR indices for each and crop to aoi
    s2_list <- rOPTRAM::optram_acquire_s2(
                    aoi_file,
                    from_date, to_date,
                    max_cloud = max_cloud,
                    veg_index = veg_index,
                    output_dir = S2_output_dir,
                    remote = remote)

    # Get full output directories for BOA, STR and NDVI
    s2_dirs <- list.dirs(S2_output_dir,  full.names = TRUE)
    BOA_dir <- s2_dirs[basename(s2_dirs) == "BOA"]
    STR_dir <- s2_dirs[basename(s2_dirs) == "STR"]
    VI_dir <- s2_dirs[basename(s2_dirs) == veg_index]

    # Calculate SWIR Transformed Reflectance was done by optram_acquire_s2()
    # STR_list <- rOPTRAM::optram_calculate_str(BOA_dir)
    STR_list <- list.files(path = STR_dir, full.names = TRUE)
    VI_list <- list.files(path = VI_dir, full.names = TRUE)
    VI_STR_df <- rOPTRAM::optram_ndvi_str(STR_list, VI_list, data_output_dir)
    rmse_list <- rOPTRAM::optram_wetdry_coefficients(
      VI_STR_df,
      aoi_file = aoi_file,
      output_dir = data_output_dir,
      step = step,
      trapezoid_method = trapezoid_method)
    message("RMSE for fitted trapezoid: \n", rmse_list)
    return(rmse_list)
}
