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
#' @param veg_index, string, which index to use. Default "NDVI"
#' @param from_date, string, the start of the date range, Formatted as "YYYY-MM-DD"
#' @param to_date, the end of the date range.
#' @param max_cloud, integer, maximum percent cloud cover, Default 15.
#' @param scihub_user, string, username on Copernicus Sentinel Hub
#' @param scihub_pass, string, password on Sentinel hub
#' @param S2_output_dir, string, directory to save downloaded S2 
#'  and the derived products, defaults to tempdir()
#' @param data_output_dir, string, path to save coeffs_file 
#'  and STR-VI data.frame, default is tempdir()
#' @param remove_safe, string, "yes" or "no", whether to delete downloaded
#'      SAFE directories after processing. Default "yes"
#' @return coeffs_file, string, full path to saved CSV of wet-dry coefficients
#' @note
#' Access to Copernicus Sentinel Hub requires registration.
#' If you have already registered, and saved your credentials
#' in the default "~/.sen2r/apihub.txt" file,
#' then you can leave the scihub_user and scihub_pass empty.
#' If your credentials are **not** yet stored, then
#' - register on the Scihub website:
#' - https://scihub.copernicus.eu/userguide/SelfRegistration
#' - enter your user and pass parameters in this function call,
#'   and they will be stored into the default location.
#' 
#' Output can be separated:
#' Sentinel downloads and products are saved to S2_output_dir.
#' Data files (Trapezoid coefficients and STR-VI data) to data_output_dir
#' @export
#' @examples
#' \dontrun{
#' from_date <- "2018-12-01"
#' to_date <- "2019-04-30"
#' aoi <- "inst/extdata/migda_9.gpkg"
#' coeffs <- optram(aoi,
#'                  from_date, to_date,
#'                  veg_index = c("SAVI"),
#'                  scihub_user = "userxxx", scihub_pass = "secretxyz"
#'                  )
#' }


optram <- function(aoi_file,
                   veg_index = 'NDVI',
                   from_date, to_date,
                   max_cloud = 15,
                   # NULL creds assumes that credentials are already
                   # stored in "~/.sen2r/apihub.txt"
                   scihub_user = NULL,
                   scihub_pass = NULL,
                   remove_safe = "yes",
                   S2_output_dir = tempdir(),
                   data_output_dir = tempdir()) {

  # Avoid "no visible binding for global variable" NOTE
  access_ok <- s2_list <- s2_dirs <- BOA_dir <- NULL
  VI_dir <- VI_list <- VI_STR_df <- coeffs  <- NULL

  # Make sure we have access to scihub
  access_ok <- rOPTRAM::check_scihub_access(scihub_user,
                                            scihub_pass)
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
                    veg_index = veg_index,
                    remove_safe = remove_safe,
                    output_dir = S2_output_dir)

    # Get full output directories for both BOA and NDVI
    s2_dirs <- list.dirs(S2_output_dir,  full.names = TRUE)
    BOA_dir <- s2_dirs[grep(pattern = "BOA", x = s2_dirs, fixed = TRUE)][1]
    STR_dir <- s2_dirs[grep(pattern = "STR", x = s2_dirs, fixed = TRUE)][1]
    VI_dir <- s2_dirs[grep(pattern = veg_index, x = s2_dirs, fixed = TRUE)][1]


    # Calculate SWIR Tranformed Reflectance
    STR_list <- rOPTRAM::optram_calculate_str(STR_dir)
    VI_list <- list.files(path = VI_dir, full.names = TRUE)
    VI_STR_df <- rOPTRAM::optram_ndvi_str(STR_list, VI_list, data_output_dir)
    coeffs <- rOPTRAM::optram_wetdry_coefficients(VI_STR_df,
                                                  aoi_file = aoi_file,
                                                  output_dir = data_output_dir)
                                                  
    return(coeffs)
}
