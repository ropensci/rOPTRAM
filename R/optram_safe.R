#' @title Handle Sentinel imagery in original Copernicus SAFE format
#'
#' @description
#' This function prepares vegetation index and STR rasters
#' from Sentinel 2 image files downloaded in advance (without using `sen2r`).
#' @param safe_dir: string, full path directory of downloaded Sentinel 2 images in SAFE format
#' @param aoi: sf object, polygon of area of interest
#' @param vi: string, which VI to prepare, either 'NVDI' or 'SAVI'
#' @param output_dir: string, where to save Geotiff, default is tempdir()
#' @return output_files:list, full paths to saved Geotif files
#' @export
#' @examples
#' print("Running optram_prepare_safe_vi_str.R")
#' 
optram_prepare_safe_vi_str <- function(safe_dir,
                          aoi,  
                          vi = 'NDVI',
                          output_dir = tempdir()) {
    # Loop over the downloaded S2 folders (dates), create NDVI and STR indices for each
    # and crop to aoi
    return(output_files)
}
