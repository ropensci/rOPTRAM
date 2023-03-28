#' @title Prepare Sentinel imagery for soil moisture prediction using OPTRAM.
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
#' @param aoi_file, string, full path to polygon spatial file of area of interest
#' @param vidx, string, which VI to prepare, either 'NVDI' or 'SAVI'
#' @param from_date, string, the start of the date range, Formatted as "YYYY-MM-DD"
#' @param to_date, the end of the date range.
#' @param max_cloud, integer, maximum percent cloud cover, Default 15.
#' @param scihub_user, string, username on Copernicus Sentinel Hub
#' @param scihub_pass, string, password on Sentinel hub
#' @param output_dir, string, directory to save coeffs_file, default is tempdir()
#' @return coeffs_file, string, full path to saved CSV of wet-dry coefficients
#' @export
#' @examples
#' print("Running optram.R")

optram <- function(aoi_file,
                   vidx = 'NDVI',
                   from_date, to_date,
                   max_cloud = 15,
                   # NULL creds assumes that credentials are already
                   # stored in "~/.sen2r/apihub.txt"
                   scihub_user = NULL,
                   scihub_pass = NULL,
                   output_dir = tempdir()) {

  # Avoid "no visible binding for global variable" NOTE
  access_ok <- s2_list <- s2_dirs <- BOA_dir <- NULL
  VI_dir <- VI_list <- VI_STR_df <- coeffs <- NULL
  
  # Make sure we have access to scihub
  optram_func <- as.character(match.call()[[1]])
  access_ok <- rOPTRAM::check_scihub_access(scihub_user, scihub_pass, optram_func)
  if (! access_ok) {
      return(NULL)
  }

    # Loop over the downloaded S2 folders (dates),
    # create NDVI and STR indices for each and crop to aoi
    s2_list <- rOPTRAM::optram_acquire_s2(
                    aoi_file,
                    from_date, to_date,
                    max_cloud = max_cloud,
                    scihub_user = scihub_user,
                    scihub_pass = scihub_pass,
                    list_indicies = vidx,
                    output_dir = output_dir)

    # Get full output directories for both BOA and NDVI
    s2_dirs <- list.dirs(output_dir,  full.names = TRUE)
    BOA_dir <- s2_dirs[grep("BOA", s2_dirs, fixed = TRUE)][1]
    VI_dir <- s2_dirs[grep(vidx, s2_dirs, fixed = TRUE)][1]

    # Calculate SWIR Tranformed Reflectance
    STR_list <- rOPTRAM::optram_calculate_str(BOA_dir)
    VI_list <- list.files(path = VI_dir, full.names = TRUE)
    VI_STR_df <- rOPTRAM::optram_ndvi_str(STR_list, VI_list)
    coeffs <- rOPTRAM::optram_wetdry_coefficients(VI_STR_df,
                                                  output_dir = output_dir)
                                                  
    return(coeffs)
}
