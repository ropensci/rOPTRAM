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
#'   \doi{https://doi.org/10.1016/j.rse.2017.05.041}
#' @param aoi, {sf} object, a POLYGON or MULTIPOLYGON, boundary of area of interest
#' @param from_date, string, the start of the date range,
#'        Formatted as "YYYY-MM-DD"
#' @param to_date, the end of the date range.
#' @param max_cloud, integer, maximum percent cloud cover, Default 15.
#' @param S2_output_dir, string, directory to save downloaded S2
#'  and the derived products, defaults to tempdir()
#' @param data_output_dir, string, path to save coeffs_file
#'  and STR-VI data.frame, default is tempdir()
#' @return rmse_df, data.frame, RMSE values of fitted trapezoid lines
#' the coefficients are also saved to a csv file in \code{data_output_dir}.
#' @note
#' Sentinel downloaded products are saved to S2_output_dir.
#' Data files (Trapezoid coefficients and STR-VI data) to data_output_dir
#'
#' Products can be downloaded covering two "period" options:
#' either "full" - all available images from \code{from_date} to \code{to_date} or
#' "seasonal" - available images for all years but only for the months between
#' day/month of \code{from_date} to the day/month of \code{to_date}.
#' Use \code{\link[rOPTRAM]{optram_options}} to set this option.
#'
#' Three trapezoid fitting methods are implemented: "linear",
#'  "exponential" and "polynomial". See the \code{\link[rOPTRAM]{optram_options}} function for details
#'
#' Two SWIR wavelength bands are available in Sentinel-2:
#' 1610 nanometer (nm) and 2190 nm.
#' The option \code{SWIR_bands} can be set in \code{\link[rOPTRAM]{optram_options}}
#' to choose which band is used in this model.
#'
#' Several vegetation indices are implemented: "NDVI", "SAVI", etc.
#' The \code{\link[rOPTRAM]{optram_options}}function also sets this option.

#' @export
#' @examples
#' \dontrun{
#' from_date <- "2018-12-01"
#' to_date <- "2020-04-30"
#' aoi <- sf::st_read(system.file("extdata",
#'                               "lachish.gpkg", package = "rOPTRAM"))
#' rmse <- optram(aoi,
#'                  from_date, to_date,
#'                  veg_index = c("SAVI"))
#' }

optram <- function(aoi,
                   from_date, to_date,
                   max_cloud = 15,
                   S2_output_dir = tempdir(),
                   data_output_dir = tempdir()) {

  # Avoid "no visible binding for global variable" NOTE
  access_ok <- s2_list <- s2_dirs <- BOA_dir <- NULL
  VI_dir <- VI_list <- VI_STR_df <- coeffs  <- NULL

  # Loop over the downloaded S2 folders (dates),
  # create NDVI and STR indices for each and crop to aoi
  s2_list <- rOPTRAM::optram_acquire_s2(
                  aoi,
                  from_date, to_date,
                  max_cloud = max_cloud,
                  output_dir = S2_output_dir)

    veg_index <- getOption("optram.veg_index")
    remote <- getOption("optram.remote")
    SWIR_band <- getOption("optram.SWIR_band")
    # Get full output directories for BOA, STR and NDVI
    s2_dirs <- list.dirs(S2_output_dir,  full.names = TRUE)
    BOA_dir <- s2_dirs[basename(s2_dirs) == "BOA"]

    # Get full output directories for BOA, STR and NDVI
    s2_dirs <- list.dirs(S2_output_dir,  full.names = TRUE)
    STR_dir <- s2_dirs[basename(s2_dirs) == "STR"]
    VI_dir <- s2_dirs[basename(s2_dirs) == veg_index]

    # Calculate SWIR Transformed Reflectance was done by optram_acquire_s2()
    # STR_list <- rOPTRAM::optram_calculate_str(BOA_dir)
    STR_list <- list.files(path = STR_dir, full.names = TRUE)
    VI_list <- list.files(path = VI_dir, full.names = TRUE)
    VI_STR_df <- rOPTRAM::optram_ndvi_str(STR_list, VI_list,
                                          data_output_dir, aoi)
    vi_step <- getOption("optram.vi_step")
    trapezoid_method <- getOption("optram.trapezoid_method")
    rmse_df <- rOPTRAM::optram_wetdry_coefficients(VI_STR_df,
                                                   output_dir = data_output_dir)
    print("RMSE for fitted trapezoid:")
    print(rmse_df)
    return(rmse_df)
}
