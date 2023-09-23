#' @title Handle Satellite imagery in original landsat format
#' @description
#' Use this function to prepares vegetation index and
#' SWIR Transformed Reflectance (STR) rasters
#' when you have already downloaded landsat image files in advance.
#' This function assumes that atmospheric correction has been applied.
#' @param landsat_dir, string, full path to containing folder of downloaded (unzipped)
#' Sentinel 2 data in original landsat format, after atompheric correction (L2A)
#' @param aoi_file, string, path to boundary polygon spatial file of area of interest
#' @param vi, string, which VI to prepare, either 'NVDI' (default) or 'SAVI' or 'MSAVI'
#' @param LC_output_dir, string, directory to save the derived products,
#'      defaults to tempdir()
#' @param data_output_dir, string, path to save coeffs_file
#'      and STR-VI data.frame, default is tempdir()
#' @return coeffs, list, the derived trapezoid coefficients
#' @export
#' @note Unlike the `optram_acquire_s2` function, there is no implementation
#'  for automatic download of Landsat images. This function requires a directory,
#'  set in the `landsat_dir` parameter,  which contains the set of Landsat tiles,
#'  downloaded manually by the user, in advance.
#'  This directory should contain folders of Landsat images, where each folder 
#'  consists of the individual Landsat bands as Geotif files, as well as the metadata files
#'  as downloaded from, i.e. the USGS EarthExplorer (https://earthexplorer.usgs.gov/) website.  
#' @examples 
#' \dontrun{optram_landsat(landsat_dir,
#'                           aoi_file,
#'                           vi = 'NDVI',
#'                           LC_output_dir = tempdir(),
#'                           data_output_dir = tempdir())
#' }

  optram_landsat <- function(landsat_dir,
                             aoi_file,
                             vi = 'NDVI',
                             LC_output_dir = tempdir(),
                             data_output_dir = tempdir()) {

    # Avoid "no visible binding for global variable" NOTE
    landsat_list <- band_ids <- aoi <- derived_rasters <- xml_file <- NULL
    img_nodes <- img_paths <- img_path <- mtd_file <- mtd <- epsg_code <- NULL
    datestr <- VI_STR_list <- stk <- VI_df <- VI_idx <- NULL
    STR <- STR_df <- full_df <- NULL

    # Loop over the downloaded LC folders (dates),
    # create NDVI and STR indices for each and crop to aoi

    landsat_list <- list.dirs(landsat_dir)[grepl("L*_02_*",list.dirs(landsat_dir))]

    # The strings below are used to select the needed bands from Landsat
    # L89 - bands for LANDSAT 8/9
    # L57 - bands for LANDSAT 5/7
    band_L89 <- c(
        "_SR_B4", #red
        "_SR_B5", #NIR wide
        "_SR_B7"  #SWIR 2200
    )
    band_L57 <- c(
      "_SR_B3", #red
      "_SR_B4", #NIR wide
      "_SR_B7"  #SWIR 2200
    )

    if (length(landsat_list) == 0) {
        message("No landsat folders in: ", landsat_dir, " directory", "\n",
        "Please check download folder.", "\n", "Exiting...")
        return(NULL)
    }

    # crs: from the first tile in the first landsat dir
    rstt <- terra::rast(dir(landsat_list[1], full.names = TRUE)[grepl("*_SR_B1.TIF$", dir(landsat_list[1]))])
    epsg_code <- paste("EPSG:",
                       (as.character(terra::crs(rstt, describe=T)[3])))
    # Get Area of interest, make sure it is projected to the CRS of Landsat images
    aoi <- terra::vect(aoi_file)
    aoi <- terra::project(aoi, epsg_code)
    aoi_name <- aoi_to_name(aoi_file)

    # Prepare output directories
    BOA_dir <- file.path(LC_output_dir, "BOA")
    if (!dir.exists(BOA_dir)) {
         dir.create(BOA_dir, recursive = TRUE)
    }

    NDVI_dir <- file.path(LC_output_dir, "NDVI")
    if (!dir.exists(NDVI_dir)) {
        dir.create(NDVI_dir)
    }

    STR_dir <- file.path(LC_output_dir, "STR")
    if (!dir.exists(STR_dir)) {
        dir.create(STR_dir)
    }

    derived_rasters <- lapply(landsat_list, function(s) {
        mtl_file <- list.files(s, pattern = "MTL.*xml$",
                                recursive = TRUE, full.names = TRUE, )[1]
        if (! file.exists(mtl_file)) {
            warning("No metadata file in landsat dir: ", s, "Skipping...")
            return(NULL)
        }
        mtl <- xml2::read_xml(mtl_file)
        # Values for gain and offset are read from the XML metadata
        # https://www.usgs.gov/faqs/how-do-i-use-a-scale-factor-landsat-level-2-science-products
        gain <- xml2::xml_text(xml2::xml_find_first(mtl, ".//REFLECTANCE_MULT_BAND_1"))
        offset <- xml2::xml_text(xml2::xml_find_first(mtl, ".//REFLECTANCE_ADD_BAND_1"))
        gain <- as.numeric(gain)
        offset <- as.numeric(offset)

        # Read in tifs
        if (grepl("LC08", s) | grepl("LC09", s)) {
          band_ids <- band_L89
        }
        if (grepl("LE07", s) | grepl("LT05", s)) {
          band_ids <- band_L57
        }
       img_list <- lapply(band_ids, function(b){
          # img_path - full names in only one landsat folder
          # filter only sr bands
          img_path <- dir(s)[grepl(pattern = "*_SR_B[0-9]*.TIF$", x = dir(s))]
          img_path <- img_path[grepl(pattern = b, img_path, fixed = TRUE)]
          img_path <- file.path(s, img_path)
          # Use the `win` parameter of `terra::rast` to crop Landsat tile to aoi.
          rst <- terra::rast(img_path, win = terra::ext(aoi))
          return(rst)
        })

        img_stk <- terra::rast(img_list)
        # All bands use the same gain, offset
        img_stk <- img_stk*gain + offset

        # Save to BOA dir
        # Create filename
        # Prepare file name parts for saving rasters
        # the name is formed from folder
        s_parts <- unlist(strsplit(basename(s), "_"))
        BOA_file <- paste(s_parts[1], s_parts[2], s_parts[3],
                        s_parts[4], s_parts[6], aoi_name,
                        "BOA.tif", sep = "_")
        terra::writeRaster(img_stk,
                         file.path(BOA_dir, BOA_file), overwrite = TRUE)
        return(img_stk)
    }) # end-of-derived_rasters

    # Get VI and STR from this list of raster stacks
    VI_STR_list <- lapply(1:length(derived_rasters), function(x) {
      # LANDSAT: Each item in the derived_rasters list is a raster stack, with 3 bands:
      # Red, NIR, SWIR 2200nm
      stk <- derived_rasters[[x]]
      if (is.null(stk)) {
        return(NULL)
      }
      # Use the metadata file from landsat directory name to get image date
      s <- landsat_list[x]
      mtl_file <- list.files(s, pattern = "MTL.*xml$",
                             recursive = TRUE, full.names = TRUE, )[1]
      if (! file.exists(mtl_file)) {
        warning("No metadata file in landsat dir: ", s, "Skipping...")
        return(NULL)
      }
      #extract time, date and create datetime
      mtl <- xml2::read_xml(mtl_file)
      datestr <- as.Date(xml2::xml_text(xml2::xml_find_first(mtl, ".//DATE_ACQUIRED")))
      # Find VI values, and create data.frame
      VI_idx <- rOPTRAM::calculate_vi(stk, vi, redband = 1, nirband = 2)
      VI_df <- terra::as.data.frame(VI_idx, xy = TRUE)
      # Add image date to data.frame
      VI_df['Date'] <- datestr
      # Now add STR values to data.frame
      STR <- rOPTRAM::calculate_str(stk, swirband = 3, scale_factor = 1)
      STR_df <- terra::as.data.frame(STR, xy = TRUE)
      full_df <- dplyr::full_join(STR_df, VI_df)
      full_df <- full_df[stats::complete.cases(full_df),]

      # Save VI to NDVI_dir
      # Prepare file name parts for saving rasters
      s_parts <- unlist(strsplit(basename(landsat_list[x]), "_"))
      VI_file <- paste(s_parts[1], s_parts[2],s_parts[3],s_parts[4],s_parts[6],
                       aoi_name, "NDVI.tif", sep = "_")
      terra::writeRaster(VI_idx,
                         file.path(NDVI_dir, VI_file), overwrite = TRUE)
      # Save STR to BOA_dir
      STR_file <- paste(s_parts[1], s_parts[2],s_parts[3],s_parts[4],s_parts[6],
                        aoi_name, "STR.tif", sep = "_")
      terra::writeRaster(STR,
                         file.path(STR_dir, STR_file), overwrite = TRUE)

      return(full_df) # end-of-VI_STR_list
    })

    full_VI_STR <- do.call(rbind, VI_STR_list)
    # Save full data.frame to work_dir as RDS (just in case, for future use)
    full_df_path <- file.path(data_output_dir, "VI_STR_data.rds")
    saveRDS(full_VI_STR, full_df_path)
    message("VI-STR data saved to: ", full_df_path)
    # Now continue with regular wet-dry coefficients process
    coeffs <- rOPTRAM::optram_wetdry_coefficients(full_VI_STR,
                                                  aoi_file,
                                                  data_output_dir)

    return(coeffs)
    }
