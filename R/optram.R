#' @title Prepare Sentinel imagery for soil moisture prediction using OPTRAM.
#'
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
#' @param aoi: string, full path to polygon spatial file of area of interest
#' @param vi: string, which VI to prepare, either 'NVDI' or 'SAVI'
#' @param coeffs_file: string, where to save coeffs_file, default is tempdir()
#' @return coeffs_files: string, full path to saved CSV of wet-dry coefficients
#' @export
#' @examples
#' print("Running prepare_s2_vi_str.R")
optram_prepare_coeffs <- function(aoi,  
                          vi = 'NDVI',
                          output_dir = tempdir()) {
    # Loop over the downloaded S2 folders (dates), create NDVI and STR indices for each
    # and crop to aoi
    return(output_files)
}
