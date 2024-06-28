#' @title Handle Sentinel Imagery in Original Copernicus SAFE Format
#' @description
#' Use this function to prepares vegetation index and
#' SWIR Transformed Reflectance (STR) rasters
#' when you have already downloaded Sentinel 2 image files in advance.
#' Unzip the downloaded Sentinel 2 files
#'      and do not change the folder structure.
#' This function assumes that atmospheric correction has been applied.
#' i.e. by downloading Level L2A product or using the SNAP L2A_Process,
#' @param safe_dir, string, full path to containing folder of downloaded
#'    (unzipped) Sentinel 2 data in original SAFE format,
#'    after atmospheric correction (L2A)
#' @param aoi, {sf} object, a POLYGON or MULTIPOLYGON of the AOI boundary
#' @param S2_output_dir, string, directory to save the derived products,
#'      defaults to tempdir()
#' @param overwrite, boolean, overwrite derived products
#'      that already were created,
#'      defaults to TRUE
#' @param data_output_dir, string, path to save coeffs_file
#'      and STR-VI data.frame, default is tempdir()
#' @return rmse_df, data.frame, RMSE values of fitted trapezoid lines
#' @export
#' @note
#' Use the \code{max_tbl_size} parameter to limit the total number of rows
#' in the VI-STR data.frame.
#' When the area of interest is large, or the time range of datasets is long,
#' the total size of the data.frame can grow
#' beyond the capacity of computation resources.
#' This parameter limits the size of the table
#' by sampling a number of data points from each time slot.
#' The sample size is determined based on \code{max_tbl_size}
#' and the total number of time slots in the full time range.
#'
#' Two SWIR bands are available in Sentinel-2: 1610 nanometer (nm) and 2190 nm.
#' Set which band to use with \code{optram_options}.
#'
#' @examples
#' \dontrun{
#' aoi <- sf::st_read(system.file("extdata",
#'                   "lachish.gpkg", package = "rOPTRAM"))
#' safe_dir  <- "...enter directory containing downloaded SAFE folders..."
#' rmse <- optram_safe(safe_dir, aoi_file)
#' }

optram_safe <- function(safe_dir,
                        aoi,
                        S2_output_dir = tempdir(),
                        overwrite = TRUE,
                        data_output_dir = tempdir()) {

    # Avoid "no visible binding for global variable" NOTE
    safe_list <- band_ids <- aoi <- cropped_rast_list <- xml_file <- NULL
    img_nodes <- img_paths <- img_path <- mtd_file <- mtd <- epsg_code <- NULL
    datestr <- VI_STR_list <- stk <- VI_df <- VI_idx <- NULL
    STR <- STR_df <- full_df <- max_tbl_size <- NULL

    # Pre flight checks...
    if (!check_aoi(aoi)) {
        return(NULL)
    }
    # Ensure aoi is single POLYGON or MULTIPOLYGON
    aoi <- sf::st_union(aoi)

    if (is.null(safe_dir) || !dir.exists(safe_dir)) {
      message("The directory of downloaded Landsat images
              is a required parameter.")
      return(NULL)
    }
    SWIR_band <- getOption("optram.SWIR_band")
    veg_index <- getOption("optram.veg_index")

    # Loop over the downloaded S2 folders (dates),
    # create NDVI and STR indices for each and crop to aoi
    safe_list <- list.dirs(safe_dir, full.names = TRUE, recursive = TRUE)
    safe_list <- safe_list[grepl(pattern = "SAFE$", x = safe_list)]
    if (length(safe_list) == 0) {
        message("No Sentinel 2 SAFE folders in: "
                , safe_dir, " directory", "\n",
                "Please check download folder.", "\n", "Exiting...")
        return(NULL)
    }

    # The strings below are used to select the needed bands from Sentinel-2
    band_ids <- c(
        "AOT_10m", #Coastal blue
        "B02_10m", #blue
        "B03_10m", #green
        "B04_10m", #red
        "B08_10m", #NIR wide
        "B05_20m", #rededge
        "B06_20m", #rededge
        "B07_20m", #rededge
        "B8A_20m", #NIR narrow
        "B11_20m", #SWIR 1600
        "B12_20m"  #SWIR 2200
    )
    # Get Area of interest as {terra} vect objcet
    aoi <- terra::vect(aoi)


    # Prepare output directories
    BOA_dir <- file.path(S2_output_dir, "BOA")
    if (!dir.exists(BOA_dir)) {
         dir.create(BOA_dir, recursive = TRUE)
    }

    VI_dir <- file.path(S2_output_dir, veg_index)
    if (!dir.exists(VI_dir)) {
        dir.create(VI_dir)
    }

    STR_dir <- file.path(S2_output_dir, "STR")
    if (!dir.exists(STR_dir)) {
        dir.create(STR_dir)
    }

    # Collect list of the BOA file paths
    # created from cropped Sentinel-2 imagery
    cropped_rast_list <- lapply(safe_list, function(s) {
        # Get file paths to 10m and 20m jpeg images
        xml_file <- list.files(s, pattern = "MTD.*xml$", full.names = TRUE)
        xml <- xml2::read_xml(xml_file)
        img_nodes <- xml2::xml_find_all(xml, ".//IMAGE_FILE")
        img_nodes <- img_nodes[!grepl(pattern = "R60m", img_nodes)]
        img_paths <- xml2::xml_contents(img_nodes)

        # Get CRS for this SAFE dataset, and reproject AOI
        mtd_file <- list.files(s, pattern = "MTD_TL.*xml$",
                                recursive = TRUE, full.names = TRUE, )[1]
        if (! file.exists(mtd_file)) {
            message("No metadata file in SAFE dir: ", s, "Skipping...")
            return(NULL)
        }
        # Get CRS for each S2 tile
        # (the list could have tiles in different UTM zones)
        mtd <- xml2::read_xml(mtd_file)
        epsg_code <- xml2::xml_text(
          xml2::xml_find_first(mtd, ".//HORIZONTAL_CS_CODE"))
        aoi <- terra::project(aoi, epsg_code)
        aoi_ext <- terra::ext(aoi)
        # Create filename
        # Prepare file name parts for saving rasters
        s_parts <- unlist(strsplit(basename(s), "_"))
        BOA_file <- paste(s_parts[1], s_parts[3], s_parts[5],
                         "BOA_10.tif", sep = "_")
        BOA_path <- file.path(BOA_dir, BOA_file)
        # Read in the bands, prepare mask and save
        # Only if BOA file does not exist, or overwrite requested
        if (!file.exists(BOA_path) || overwrite == TRUE) {
            # Read in jp2 files
            img_list <- lapply(img_paths, function(i){
                img_file <- paste0(i, ".jp2")
                img_path <- file.path(s, img_file)
                rst <- terra::rast(img_path)
                rst <- terra::mask(terra::crop(rst, aoi), aoi)
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
            names(img_stk) <- band_ids
            # Save to BOA dir
            terra::writeRaster(img_stk, BOA_path, overwrite = TRUE)
        }
        img_stk <- NULL
        return(BOA_path)
    })

    # Get index of rows for sampling
    # Use the first raster (first date) in derived rasters
    # Get the red band (3) and
    # use that to determine index for random sampling
    r <- terra::rast(cropped_rast_list[[1]])[[3]]
    r_df <- terra::as.data.frame(r, xy = TRUE)
    # Make sure length of data.frame is less than:
    # max_tbl_size / number of raster dates.
    # Other wise, take a sample
    if (nrow(r_df) > (max_tbl_size / length(cropped_rast_list))) {
        # Set sample size as:
        # maximum table / number of dates in date range
        samp_size <- max_tbl_size / length(cropped_rast_list)
        idx <- sample(nrow(r_df), samp_size)
    } else {
        idx <- seq(1, nrow(r_df))
    }

    # Get VI and STR from this list of raster stacks
    VI_STR_list <- lapply(seq_along(cropped_rast_list), function(x) {
        # Each item in the cropped_rast_list list is a raster stack,
        # with 6 bands: R-G-B-NIR, SWIR 1600, SWIR 2200
        stk <- terra::rast(cropped_rast_list[[x]])
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
            message("No metadata file in SAFE dir: ", s, "Skipping...")
            return(NULL)
        }
        mtd <- xml2::read_xml(mtd_file)
        datestr <- as.Date(xml2::xml_text(xml2::xml_find_first(mtd,
                            ".//SENSING_TIME")))

        VI_idx <- rOPTRAM::calculate_vi(stk,
                                        redband = 3, nirband = 4,
                                        blueband = 1, greenband = 2)
        VI_df <- terra::as.data.frame(VI_idx, xy = TRUE, na.rm = FALSE)
        # Add image date to dataframe
        VI_df['Date'] <- datestr
        # Get the subset sample using the idx values from above
        VI_df <- VI_df[idx, ]

        STR <- rOPTRAM::calculate_str(stk, SWIR_band = SWIR_band)
        STR_df <- terra::as.data.frame(STR, xy = TRUE,  na.rm = FALSE)
        # Get the subset sample using the idx values from above
        STR_df <- STR_df[idx, ]

        full_df <- dplyr::full_join(STR_df, VI_df, by = c("x", "y"))
        full_df <- full_df[stats::complete.cases(full_df),]

        # Save VI to NDVI_dir
        # Prepare file name parts for saving rasters
        s_parts <- unlist(strsplit(basename(safe_list[x]), "_"))
        VI_file <- paste(s_parts[1], s_parts[3], s_parts[5],
                        "VI_10.tif", sep = "_")
        VI_path <- file.path(VI_dir, VI_file)
        if (!file.exists(VI_path) || overwrite == TRUE) {
            terra::writeRaster(VI_idx, VI_path, overwrite = TRUE)
        }
        # Save STR to BOA_dir
        STR_file <- paste(s_parts[1], s_parts[3], s_parts[5],
                        "STR_10.tif", sep = "_")
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
    rmse_df <- optram_wetdry_coefficients(full_VI_STR,
                                          output_dir = data_output_dir)
    message("RMSE for fitted trapezoid:")
    print(rmse_df)
    return(rmse_df)
}
