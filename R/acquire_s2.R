#' Acquire Sentinel 2 images at a given location and date range
#'
#' @param aoi: sf object, polygon boundary of area of interest
#' @param from_date: string, represents start of date range, formatted as "YYYY-MM-DD"
#' @param to_date: string, end of date range, formatted as "YYYY-MM-DD"
#' @return output_path: string, path to downloaded files
#' @export
#' @examples print("acquire_s2_images.R")
#'

acquire_s2_images <- function(aoi, from_date, to_date, output_dir = tempdir()) {
    # Download Sentinel 2 images during the requested date range,
    # and clip to the area of interest
    if (!require(sen2r)) {
        warning("This function requires the `sen2r` package.", "\n",
        "Please install that package before running `acquire_s2_images.R`")
    }
}

prepare_s2_ndvi <- function(output_dir) {
    # Loop over the downloaded S2 images, and create NDVI and STR indices

    }
