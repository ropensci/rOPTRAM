#' @title Handle Third Party Imagery With Red, NIR and SWIR Bands
#' @description
#' This function prepares vegetation index and STR rasters
#' from other (non-Sentinel) image files downloaded in advance.
#' @param img_dir, string, full path directory of downloaded images
#'                 (assumes Geotiff)
#' @param aoi, string, full path to polygon spatial file of area of interest
#' @param viname, string, which VI to prepare, 'NDVI', 'SAVI', etc.
#' @param output_dir, string, where to save Geotiff, default is tempdir()
#' @return output_files, list, full paths to saved Geotiff files
#' (not exported yet)
#' @examples
#' print("Running prepare_other_vi_str.R")

optram_prepare_other_vi_str <- function(img_dir,
                          aoi,
                          viname = 'NDVI',
                          output_dir = tempdir()) {
    output_files <- NULL
    # Loop over the download folder (dates),
    # create NDVI and STR indices for each
    # and crop to aoi
    return(output_files)
}
