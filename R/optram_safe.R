#' @title Handle Sentinel imagery in original Copernicus SAFE format
#'
#' @description
#' Use this function to prepares vegetation index and STR rasters 
#' when you have already downloaded Sentinel 2 image files in advance
#' (without using `sen2r`).
#' Unzip the downloaded Sentinel 2 files and do not change the folder structure
#' 
#' @param safe_dir: string, full path to containing folder of downloaded (unzipped)
#' Sentinel 2 data in original SAFE format
#' @param aoi_file: string, path to boundary polygon spatial file of area of interest
#' @param vi: string, which VI to prepare, either 'NVDI' or 'SAVI'
#' @param output_dir: string, where to save Geotiff, default is tempdir()
#' 
#' @return output_files:list, full paths to saved Geotif files
#' @export
#' @examples
#' print("Running optram_prepare_safe_vi_str.R")
#' 
optram_prepare_safe_vi_str <- function(safe_dir,
                          aoi_file,
                          vi = 'NDVI',
                          output_dir = tempdir()) {
    # Loop over the downloaded S2 folders (dates), create NDVI and STR indices for each
    # and crop to aoi
    safe_list <- list.dirs(safe_dir, full.names = TRUE, recursive = TRUE)
    safe_list <- safe_list[grepl(pattern = "SAFE$", x = safe_list)]
    if (length(safe_list) == 0) {
        message("No Sentinel 2 SAFE folders in: ", safe_dir, " directory", "\n",
        "Please check download folder.", "\n", "Exiting...")
        return(NULL)
    }
    img_list <- safe_list[grepl(pattern = "IMG_DATA", safe_list, fixed = TRUE)]
    # ignore the 60 meter resolution
    img_list <- img_list[!grepl(pattern = "R60m$", x = img_list)]
    if (length(img_list) == 0) {
        message("No image data folders in any SAFE directory")
        return(NULL)
    }

    # Find coordinate reference system of this Sentinel data
    # Then read boundary polygon and reproject to Sentinel
    if (require("xml2")) {
        # Get Sentinel CRS from metadata
        mtd_files <- list.files(safe_dir,
                                recursive = TRUE,
                                full.names = TRUE,
                                pattern = "MTD_TL.xml$")
        mtd <- xml2::read_xml(mtd_files[1])
        epsg_code <- xml2::xml_text(xml2::xml_find_first(mtd, ".//HORIZONTAL_CS_CODE"))
    } else {
        # Read in one S2 data file, and get CRS from that raster

    }
    # Read boundary polygon and tranform to the Sentinel CRS
    aoi <- terra::vect(aoi_file)
    aoi <- terra::project(aoi, epsg_code)
    
    vis_list <- list.files(path = img_list[grepl(pattern = "R10m$", x = img_list)])
    aoi  <- terra::vect(aoi_file)
    red_list <- lapply(vis_list, function(j) {
        red_file <- grep("_B04_", vis_list[j], ignore.case = FALSE, perl = FALSE)
        red <- terra::rast(red_file)
        red <- terra::mask(terra::crop(red), red)
    })
    green_list <- 
    swir_list <- img_list[grepl(pattern = "R20m$")]

    return(output_files)
}
