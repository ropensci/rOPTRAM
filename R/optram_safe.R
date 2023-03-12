#' @title Handle Sentinel imagery in original Copernicus SAFE format
#'
#' @description
#' Use this function to prepares vegetation index and
#' SWIR Transformed Reflectance (STR) rasters
#' when you have already downloaded Sentinel 2 image files in advance
#' (without using `sen2r`).
#' Unzip the downloaded Sentinel 2 files and do not change the folder structure
#' 
#' @param safe_dir: string, full path to containing folder of downloaded (unzipped)
#' Sentinel 2 data in original SAFE format
#' @param aoi_file: string, path to boundary polygon spatial file of area of interest
#' @param vi: string, which VI to prepare, either 'NVDI' (default) or 'SAVI' or 'MSAVI'
#' @param output_dir: string, where to save VI and STR and Geotiff, default is tempdir()
#'
#' @return output_files:list, full paths to saved Geotif files
#' @export
#' @examples
#' print("Running optram_prepare_safe_vi_str.R")
#'
optram_safe <- function(safe_dir,
                        aoi_file,
                        vi = 'NDVI',
                        output_dir = tempdir()) {
    # Loop over the downloaded S2 folders (dates), create NDVI and STR indices for each
    # and crop to aoi
    safe_list <- list.dirs(safe_dir, full.names = TRUE, recursive = TRUE)
    safe_list <- safe_list[grepl(pattern = "SAFE$", x = safe_list)]
    band_ids <- c(
    #"AOT_10m", #Coastal blue
    "B02_10m", #blue
    "B03_10m", #green
    "B04_10m", #red
    "B08_10m", #NIR wide
    #"B05_20m", #rededge
    #"B06_20m", #rededge
    #"B07_20m", #rededge
    #"B8A_20m", #NIR narrow
    "B11_20m", #SWIR 1600
    "B12_20m"  #SWIR 2200
    )
    # Get Area of interest
    aoi <- terra::vect(aoi_file)

    if (length(safe_list) == 0) {
        message("No Sentinel 2 SAFE folders in: ", safe_dir, " directory", "\n",
        "Please check download folder.", "\n", "Exiting...")
        return(NULL)
    }

    derived_rasters <- lapply(safe_list, function(s){
        xml_file <- list.files(s, pattern = "MTD.*xml$", full.names = TRUE)
        xml <- xml2::read_xml(xml_file)
        img_nodes <- xml2::xml_find_all(xml, ".//IMAGE_FILE")
        img_nodes <- img_nodes[!grepl(pattern = "R60m", img_nodes)]
        
        # Get CRS for this SAFE dataset, and reproject AOI 
        mtd_file <- list.files(s, pattern = "MTD_TL.*xml$",
                                recursive = TRUE, full.names = TRUE, )
        if (! file.exists(mtd_file)) {
            warning("No metadata file in SAFE dir: ", s, "Skipping...")
            break()
        } 
        mtd <- xml2::read_xml(mtd_files[1])
        epsg_code <- xml2::xml_text(xml2::xml_find_first(mtd, ".//HORIZONTAL_CS_CODE"))
        aoi <- terra::project(aoi, epsg_code)

        # Read in jp2 files
        img_list <- lapply(band_ids, function(b){
            img_node <- img_nodes[grepl(pattern = b, img_nodes, fixed = TRUE)]
            img_file <- paste0(xml2::xml_text(img_node), ".jp2")
            img_path <- file.path(s, img_file)
            r <- terra::rast(img_path, win = terra::ext(aoi))
        })
        # Make a rast obj to save the high resolution extent
        img_10m <- terra::rast()
        img_10m_list <- lapply(img_list, function(i) {
            if (all.equal.list(terra::res(i))) {
                img_10m  <<- i 
                return(i)
            } else {
                return(terra::resample(i, img_10m,
                                      method = "bilinear", threads = TRUE))
            }
        })
        img_stk <- terra::rast(img_10m_list)
        return(img_stk)
    })

    # Get VI and STR from this list of raster stacks
    vi <- rOPTRAM::calculate_vi(img_stk, vi, redband = 3, nirband = 4)
}

