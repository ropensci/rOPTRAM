#' @title Handle Sentinel imagery in original Copernicus SAFE format
#' @description
#' Use this function to prepares vegetation index and
#' SWIR Transformed Reflectance (STR) rasters
#' when you have already downloaded Sentinel 2 image files in advance
#' (without using `sen2r`).
#' Unzip the downloaded Sentinel 2 files and do not change the folder structure.
#' This function assumes that atmospheric correction has been applied.
#' i.e. using the SNAP L2A_Process,
#' or the  `sen2cor()` function from the {sen2r} R package.
#' @param safe_dir, string, full path to containing folder of downloaded
#'    (unzipped) Sentinel 2 data in original SAFE format,
#'    after atompheric correction (L2A)
#' @param aoi_file, string, path to boundary polygon spatial file
#'    of area of interest
#' @param veg_index, string, which VI to prepare, either 'NVDI' (default)
#'    or 'SAVI' or 'MSAVI'
#' @param S2_output_dir, string, directory to save the derived products,
#'      defaults to tempdir()
#' @param overwrite, boolean, overwrite derived products
#'      that already were created,
#'      defaults to TRUE
#' @param data_output_dir, string, path to save coeffs_file
#'      and STR-VI data.frame, default is tempdir()
#' @param max_tbl_size, numeric, maximum size of VI/STR dta.frame.
#'      default is 5,000,000 rows
#' @return coeffs, list, the derived trapezoid coefficients
#' @export
#' @note
#' Use the `max_tbl_size` parameter to limit the total number of rows
#' in the VI-STR data.frame.
#' When the area of interest is large, or the time range of datasets is long,
#' the total size of the data.frame can grow
#' beyond the capacity of computation resources.
#' This parameter limits the size of the table
#' by sampling a number of data points from each time slot.
#' The sample size is determined based on `max_tbl_size`
#' and the total number of time slots in the full time range.
#' @examples
#' \dontrun{
#' aoi_file <- system.file("extdata", "migda_aoi.gpkg", package = "rOPTRAM")
#' safe_dir  <- tempdir()
#' coeffs <- optram_safe(safe_dir, aoi_file, veg_index = "SAVI")
#' }

optram_safe <- function(safe_dir,
                        aoi_file,
                        veg_index = 'NDVI',
                        S2_output_dir = tempdir(),
                        overwrite = TRUE,
                        data_output_dir = tempdir(),
                        max_tbl_size = 5e+6) {

    # Avoid "no visible binding for global variable" NOTE
    safe_list <- band_ids <- aoi <- derived_rasters <- xml_file <- NULL
    img_nodes <- img_paths <- img_path <- mtd_file <- mtd <- epsg_code <- NULL
    datestr <- VI_STR_list <- stk <- VI_df <- VI_idx <- NULL
    STR <- STR_df <- full_df <- NULL

    # Pre flight checks...
    if (!check_aoi(aoi_file)) {
        return(NULL)
    }

    if (is.null(safe_dir) || !dir.exists(safe_dir)) {
      warning("The directory of downloaded Landsat images
              is a required parameter.")
      return(NULL)
    }

    # Loop over the downloaded S2 folders (dates),
    # create NDVI and STR indices for each and crop to aoi
    safe_list <- list.dirs(safe_dir, full.names = TRUE, recursive = TRUE)
    safe_list <- safe_list[grepl(pattern = "SAFE$", x = safe_list)]
    if (length(safe_list) == 0) {
        message("No Sentinel 2 SAFE folders in: ", safe_dir, " directory", "\n",
        "Please check download folder.", "\n", "Exiting...")
        return(NULL)
    }

    # The strings below are used to select the needed bands from Sentinel
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
    aoi_name <- aoi_to_name(aoi_file)

    # Prepare output directories
    BOA_dir <- file.path(S2_output_dir, "BOA")
    if (!dir.exists(BOA_dir)) {
         dir.create(BOA_dir, recursive = TRUE)
    }

    VI_dir <- file.path(S2_output_dir, "VI")
    if (!dir.exists(VI_dir)) {
        dir.create(VI_dir)
    }

    STR_dir <- file.path(S2_output_dir, "STR")
    if (!dir.exists(STR_dir)) {
        dir.create(STR_dir)
    }

    # Collect list of the BOA file paths created from cropped Sentinel-2 imagery 
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
        epsg_code <- xml2::xml_text(
          xml2::xml_find_first(mtd, ".//HORIZONTAL_CS_CODE"))
        aoi <- terra::project(aoi, epsg_code)
        aoi_ext <- terra::ext(aoi)

        # Read in jp2 files
        img_list <- lapply(band_ids, function(b){
            img_path <- img_paths[grepl(pattern = b, img_paths, fixed = TRUE)]
            img_file <- paste0(img_path, ".jp2")
            img_path <- file.path(s, img_file)
            rst <- terra::rast(img_path)
            rst <- terra::mask(terra::crop(rst, aoi_ext), aoi_ext)
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
        # Prepare file name parts for saving rasters
        s_parts <- unlist(strsplit(basename(s), "_"))
        BOA_file <- paste(s_parts[1], s_parts[3], s_parts[5],
                        aoi_name, "BOA_10.tif", sep = "_")
        BOA_path <- file.path(BOA_dir, BOA_file)
        if (!file.exists(BOA_path) || overwrite == TRUE) {
            terra::writeRaster(img_stk, BOA_path, overwrite = TRUE)
        }
        img_stk <- NULL
        return(BOA_file)
    })

    # Get index of rows for sampling
    # Use the first raster (first date) in derived rasters
    # Get the red band (3) and
    # use that to determine index for random sampling
    r <- rast(derived_rasters[[1]])[[3]]
    r_df <- as.data.frame(r, xy = TRUE)
    if (nrow(r_df) > max_tbl_size) {
        # Set sample size as:
        # maximum table / number of dates in date range
        samp_size <- max_tbl_size / length(derived_rasters)
        idx <- sample(nrow(r_df), samp_size)
    } else {
        idx <- seq(1, nrow(r_df))
    }

    # Get VI and STR from this list of raster stacks
    VI_STR_list <- lapply(seq_along(derived_rasters), function(x) {
        # Each item in the derived_rasters list is a raster stack, with 6 bands
        # R-G-B-NIR, SWIR 1600, SWIR 2200
        stk <- rast(derived_rasters[[x]])
        if (is.null(stk)) {
            return(NULL)
        }

        # Use the metadata file from SAFE directory name to get image date
        s <- safe_list[x]
        # Prepare file name parts for saving rasters
        s_parts <- unlist(strsplit(basename(s), "_"))
        mtd_file <- list.files(s, pattern = "MTD_TL.*xml$",
                                recursive = TRUE, full.names = TRUE, )[1]
        if (! file.exists(mtd_file)) {
            warning("No metadata file in SAFE dir: ", s, "Skipping...")
            return(NULL)
        }
        mtd <- xml2::read_xml(mtd_file)
        datestr <- as.Date(xml2::xml_text(xml2::xml_find_first(mtd,
                            ".//SENSING_TIME")))

        VI_idx <- rOPTRAM::calculate_vi(stk, veg_index,
                                        redband = 3, nirband = 4,
                                        blueband = 1, greenband = 2)
        VI_df <- terra::as.data.frame(VI_idx, xy = TRUE)
        # Add image date to dataframe
        VI_df['Date'] <- datestr
        # Get the subset sample using the idx values from above
        VI_df <- VI_df[idx, ]

        STR <- rOPTRAM::calculate_str(stk, swirband = 5)
        STR_df <- terra::as.data.frame(STR, xy = TRUE)
        # Get the subset sample using the idx values from above
        STR_df <- STR_df[idx, ]

        full_df <- dplyr::full_join(STR_df, VI_df)
        full_df <- full_df[stats::complete.cases(full_df),]

        # Save VI to NDVI_dir
        # Prepare file name parts for saving rasters
        s_parts <- unlist(strsplit(basename(safe_list[x]), "_"))
        VI_file <- paste(s_parts[1], s_parts[3], s_parts[5],
                        aoi_name, "VI_10.tif", sep = "_")
        VI_path <- file.path(VI_dir, VI_file)
        if (!file.exists(VI_path) || overwrite == TRUE) {
            terra::writeRaster(VI_idx, VI_path, overwrite = TRUE)
        }
        # Save STR to BOA_dir
        STR_file <- paste(s_parts[1], s_parts[3], s_parts[5],
                        aoi_name, "STR_10.tif", sep = "_")
        STR_path <- file.path(STR_dir, STR_file)
        if (!file.exists(STR_path) || overwrite == TRUE) {
        terra::writeRaster(STR, STR_path, overwrite = TRUE)
        }
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
