#' @title Handle Sentinel imagery in original Copernicus SAFE format
#' @description
#' Use this function to prepares vegetation index and
#' SWIR Transformed Reflectance (STR) rasters
#' when you have already downloaded Sentinel 2 image files in advance
#' (without using `sen2r`).
#' Unzip the downloaded Sentinel 2 files and do not change the folder structure
#' @param safe_dir, string, full path to containing folder of downloaded (unzipped)
#' Sentinel 2 data in original SAFE format
#' @param aoi_file, string, path to boundary polygon spatial file of area of interest
#' @param vi, string, which VI to prepare, either 'NVDI' (default) or 'SAVI' or 'MSAVI'
#' @param S2_output_dir, string, directory to save downloaded S2 
#'      and the derived products, defaults to tempdir()
#' @param data_output_dir, string, path to save coeffs_file 
#'      and STR-VI data.frame, default is tempdir()
#' @return coeffs, list, the derived trapezoid coefficients
#' @export
#' @examples
#' print("Running optram_prepare_safe_vi_str.R")

optram_safe <- function(safe_dir,
                        aoi_file,
                        vi = 'NDVI',
                        S2_output_dir = tempdir(),
                        data_output_dir = tempdir()) {

    # Avoid "no visible binding for global variable" NOTE
    safe_list <- band_ids <- aoi <- derived_rasters <- xml_file <- NULL
    img_nodes <- img_paths <- img_path <- mtd_file <- mtd <- epsg_code <- NULL
    datestr <- VI_STR_list <- stk <- VI_df <- VI_idx <- NULL
    STR <- STR_df <- full_df <- NULL
    
    # Loop over the downloaded S2 folders (dates),
    # create NDVI and STR indices for each and crop to aoi
    safe_list <- list.dirs(safe_dir, full.names = TRUE, recursive = TRUE)
    safe_list <- safe_list[grepl(pattern = "SAFE$", x = safe_list)]
    band_ids <- c(
    #"AOT_10m", #Coastal blue
    "B02", #blue
    "B03", #green
    "B04", #red
    "B08", #NIR wide
    #"B05_20m", #rededge
    #"B06_20m", #rededge
    #"B07_20m", #rededge
    #"B8A_20m", #NIR narrow
    "B11", #SWIR 1600
    "B12"  #SWIR 2200
    )
    # Get Area of interest
    aoi <- terra::vect(aoi_file)

    if (length(safe_list) == 0) {
        message("No Sentinel 2 SAFE folders in: ", safe_dir, " directory", "\n",
        "Please check download folder.", "\n", "Exiting...")
        return(NULL)
    }

    # Prepare file name parts for saving rasters
    s_parts <- unlist(strsplit(basename(safe_list[1]), "_"))
    aoi_name <- aoi_to_name(aoi_file)
    
    # Prepare output directories
    BOA_dir <- file.path(S2_output_dir, "BOA")
    if (!dir.exists(BOA_dir)) {
         dir.create(BOA_dir, recursive = TRUE)
    }
    
    NDVI_dir <- file.path(S2_output_dir, "NDVI")
    if (!dir.exists(NDVI_dir)) {
        dir.create(NDVI_dir)
    }

    STR_dir <- file.path(S2_output_dir, "STR")
    if (!dir.exists(STR_dir)) {
        dir.create(STR_dir)
    }
    
    derived_rasters <- lapply(safe_list, function(s) {
        xml_file <- list.files(s, pattern = "MTD.*xml$", full.names = TRUE)
        xml <- xml2::read_xml(xml_file)
        img_nodes <- xml2::xml_find_all(xml, ".//IMAGE_FILE")
        img_nodes <- img_nodes[!grepl(pattern = "R60m", img_nodes)]
        img_paths <- xml2::xml_contents(img_nodes)

        # Get CRS for this SAFE dataset, and reproject AOI 
        mtd_file <- list.files(s, pattern = "MTD_TL.*xml$",
                                recursive = TRUE, full.names = TRUE, )[1]
        if (! file.exists(mtd_file)) {
            warning("No metadata file in SAFE dir: ", s, "Skipping...")
            return(NULL)
        }
        mtd <- xml2::read_xml(mtd_file)
        epsg_code <- xml2::xml_text(xml2::xml_find_first(mtd, ".//HORIZONTAL_CS_CODE"))
        aoi <- terra::project(aoi, epsg_code)

        # Read in jp2 files
        img_list <- lapply(band_ids, function(b){
            img_path <- img_paths[grepl(pattern = b, img_paths, fixed = TRUE)]
            img_file <- paste0(img_path, ".jp2")
            img_path <- file.path(s, img_file)
            rst <- terra::rast(img_path, win = terra::ext(aoi))
            return(rst)
        })
        # Make a rast obj to save the high resolution extent
        # The first raster in the list is blue, 10m. Use for reampling
        img_10m <- img_list[[1]]
        img_10m_list <- lapply(img_list, function(i) {
            if (all(terra::res(i) == c(10, 10))) {
                return(i)
            } else {
                return(terra::resample(i, img_10m,
                                      method = "bilinear",
                                      threads = TRUE))
            }
        })
        img_stk <- terra::rast(img_10m_list)
        # Save to BOA dir
        # Create filename
        BOA_file <- paste(s_parts[1], s_parts[3], s_parts[5],
                        aoi_name, "BOA_10.tif", sep = "_")
        terra::writeRaster(img_stk,
                         file.path(BOA_dir, BOA_file), overwrite = TRUE)
        return(img_stk)
    })

    # Get VI and STR from this list of raster stacks
    VI_STR_list <- lapply(1:length(derived_rasters), function(x) {
        # Each item in the derived_rasters list is a raster stack, with 6 bands
        # R-G-B-NIR, SWIR 1600, SWIR 2200
        stk <- derived_rasters[[x]]
        if (is.null(stk)) {
            return(NULL)
        }

        # Use the metadata file from SAFE directory name to get image date
        s <- safe_list[x]
        mtd_file <- list.files(s, pattern = "MTD_TL.*xml$",
                                recursive = TRUE, full.names = TRUE, )[1]
        if (! file.exists(mtd_file)) {
            warning("No metadata file in SAFE dir: ", s, "Skipping...")
            return(NULL)
        }
        mtd <- xml2::read_xml(mtd_file)
        datestr <- as.Date(xml2::xml_text(xml2::xml_find_first(mtd, ".//SENSING_TIME")))
        #datetime <- strptime(datestr, format = "%FT%X", tz = "UTC")

        VI_idx <- rOPTRAM::calculate_vi(stk, vi, redband = 3, nirband = 4)
        VI_df <- terra::as.data.frame(VI_idx, xy = TRUE)
        # Add image date to dataframe
        VI_df['Date'] <- datestr
        
        STR <- rOPTRAM::calculate_str(stk, swirband = 5)
        STR_df <- terra::as.data.frame(STR, xy = TRUE)
        full_df <- dplyr::full_join(STR_df, VI_df)
        full_df <- full_df[stats::complete.cases(full_df),]

        # Save VI to NDVI_dir
        VI_file <- paste(s_parts[1], s_parts[3], s_parts[5],
                        aoi_name, "NDVI_10.tif", sep = "_")
        terra::writeRaster(VI_idx,
                         file.path(NDVI_dir, VI_file), overwrite = TRUE)
        # Save STR to BOA_dir
        STR_file <- paste(s_parts[1], s_parts[3], s_parts[5],
                        aoi_name, "STR_10.tif", sep = "_")
        terra::writeRaster(STR,
                        file.path(STR_dir, STR_file), overwrite = TRUE)

        return(full_df)
    })
    full_VI_STR <- do.call(rbind, VI_STR_list)
    # SAve full data.frame to work_dir
    full_df_path <- file.path(data_output_dir, "VI_STR_data.rds")
    saveRDS(full_VI_STR, full_df_path)
    message("VI-STR data saved to: ", full_df_path)
    # Now continue with regular process
    coeffs <- rOPTRAM::optram_wetdry_coefficients(full_VI_STR,
                                                  aoi_file,
                                                  data_output_dir)

    return(coeffs)
}
