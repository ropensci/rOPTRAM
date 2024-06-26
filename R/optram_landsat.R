#' @title Handle Satellite Imagery in Original Landsat Format
#' @description
#' Use this function to prepare vegetation index and SWIR Transformed Reflectance (STR) rasters
#' when you have already downloaded Landsat image files in advance.
#' This function assumes that atmospheric correction has been applied.
#' @param landsat_dir, string, full path to containing folder of downloaded
#'    (unzipped) Landsat data in original landsat format, after atmospheric correction (L2A)
#' @param aoi, {sf} object, POLYGON or MULTIPOLYGON of AOI boundary
#'    of area of interest
#' @param LC_output_dir, string, directory to save the derived products,
#'      defaults to tempdir()
#' @param data_output_dir, string, path to save coeffs_file
#'      and STR-VI data.frame, default is tempdir()
#' @return rmse_df, data.frame, RMSE values of fitted trapezoid edges
#' @export
#' @note Unlike the \code{\link[rOPTRAM]{optram_acquire_s2}} function, there is no implementation for automatic download of Landsat images.
#'  This function requires a directory, set in the \code{landsat_dir} parameter,
#'  which contains the set of Landsat tiles downloaded manually by the user, in advance.
#'  This directory should contain folders of Landsat images, where each folder
#'  consists of the individual Landsat bands as Geotiff files, as well as the metadata files as downloaded from,
#'  i.e. the USGS EarthExplorer \url{https://earthexplorer.usgs.gov/} website.
#' 
#' @examples
#' \dontrun{
#' aoi <- sf::st_read(system.file("extdata",
#'                               "lachish.gpkg", package = "rOPTRAM"))
#' # landsat_dir is a directory containing the original downloaded Landsat images.
#' landsat_dir <- "...enter full path here..."
#' optram_landsat(landsat_dir,  aoi,
#'                veg_index = 'SAVI',
#'                LC_output_dir = tempdir(), data_output_dir = tempdir())
#' }

optram_landsat <- function(landsat_dir,
                           aoi,
                           LC_output_dir = tempdir(),
                           data_output_dir = tempdir()) {

  # Avoid "no visible binding for global variable" NOTE
  landsat_dir_list <- band_ids <- aoi <- cropped_rast_list <- xml_file <- NULL
  img_nodes <- img_paths <- img_path <- mtd_file <- mtd <- epsg_code <- NULL
  datestr <- VI_STR_list <- stk <- VI_df <- VI_idx <- NULL
  STR <- STR_df <- full_df <- NULL

  # Get vegetation index from package options
  veg_index <- getOption("optram.veg_index")
  # Check for inputs
  if (is.null(landsat_dir) || !dir.exists(landsat_dir)) {
    message("The directory of downloaded Landsat images
                is a required parameter.")
    return(NULL)
  }
  if (!check_aoi(aoi)) {
    return(NULL)
  }
  # Ensure aoi is single POLYGON or MULTIPOLYGON
  aoi <- sf::st_union(aoi)

  # The strings below are used to select the needed bands from Landsat
  # L89 - bands for LANDSAT 8/9
  # L457 - bands for LANDSAT 4/5/7
  band_L89 <- c(
    "_SR_B4", #red
    "_SR_B5", #NIR wide
    "_SR_B7"  #SWIR 2200
  )
  band_L457 <- c(
    "_SR_B3", #red
    "_SR_B4", #NIR wide
    "_SR_B7"  #SWIR 2200
  )

  # crs: from the first tile in the first landsat dir
  landsat_dir_list <- grep("L*02*", list.dirs(landsat_dir), value = TRUE)
  LS_b1 <- grep("*_B1.TIF$", list.files(landsat_dir_list[1], full.names = TRUE),
                value = TRUE)
  rstt <- terra::rast(LS_b1)
  epsg_code <- paste("EPSG:",
                    (as.character(terra::crs(rstt, describe = TRUE)[3])))
  # Get Area of interest,
  # make sure it is projected to the CRS of Landsat images
  aoi <- sf::st_zm(aoi, drop = TRUE, what = "zm")
  aoi <- terra::vect(aoi)
  aoi <- terra::project(aoi, epsg_code)

  # Prepare output directories
  BOA_dir <- file.path(LC_output_dir, "BOA")
  if (!dir.exists(BOA_dir)) {
       dir.create(BOA_dir, recursive = TRUE)
  }

  VI_dir <- file.path(LC_output_dir, veg_index)
  if (!dir.exists(VI_dir)) {
      dir.create(VI_dir)
  }

  STR_dir <- file.path(LC_output_dir, "STR")
  if (!dir.exists(STR_dir)) {
      dir.create(STR_dir)
  }
  cropped_rast_list <- crop_landsat_list(landsat_dir)

  # Get VI and STR from this list of raster stacks
  VI_STR_list <- lapply(seq_along(cropped_rast_list), function(x) {
    # LANDSAT: Each item in the cropped_rast_list list
    # is a raster stack, with 3 bands: Red, NIR, SWIR 2200nm
    stk <- cropped_rast_list[[x]]
    if (is.null(stk)) {
      return(NULL)
    }
    # Use the metadata file from landsat directory name to get image date
    s <- landsat_dir_list[x]
    mtl_file <- list.files(s, pattern = "MTL.*xml$",
                           recursive = TRUE, full.names = TRUE, )[1]
    if (! file.exists(mtl_file)) {
      message("No metadata file in landsat dir: ", s, "Skipping...")
      return(NULL)
    }
    #extract time, date and create datetime
    mtl <- xml2::read_xml(mtl_file)
    datestr <- as.Date(xml2::xml_text(
      xml2::xml_find_first(mtl, ".//DATE_ACQUIRED")))
    # Find VI values, and create data.frame
    VI_idx <- rOPTRAM::calculate_vi(stk, veg_index,
                                    redband = 1, nirband = 2,
                                    scale_factor = 255)
    VI_df <- terra::as.data.frame(VI_idx, xy = TRUE)
    # Add image date to data.frame
    VI_df['Date'] <- datestr
    # Now add STR values to data.frame
    STR <- rOPTRAM::calculate_str(stk, swirband = 3, scale_factor = 1)
    STR_df <- terra::as.data.frame(STR, xy = TRUE)
    full_df <- dplyr::full_join(STR_df, VI_df)
    full_df <- full_df[stats::complete.cases(full_df),]
    
    # Save VI to VI_dir
    # Prepare file name parts for saving rasters
    s_parts <- unlist(strsplit(basename(landsat_dir_list[x]), "_"))
    VI_file <- paste(s_parts[1], s_parts[2],s_parts[3],s_parts[4],s_parts[6],
                     "NDVI.tif", sep = "_")
    terra::writeRaster(VI_idx,
                       file.path(VI_dir, VI_file), overwrite = TRUE)
    # Save STR to BOA_dir
    STR_file <- paste(s_parts[1], s_parts[2],
                      s_parts[3],s_parts[4],s_parts[6],
                      "STR.tif", sep = "_")
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
  rmse_df <- rOPTRAM::optram_wetdry_coefficients(full_VI_STR,
                                                 data_output_dir)

  return(rmse_df)
  }


#' @title Crop List of Landsat Bands to AOI
#' @description
#' Utility function to prepare BOA bands cropped to Area of Interest.
#' @param landsat_dir, string, directory containing the original downloaded Landsat imagery folders.
#' @return cropped_list, list, paths to derived BOA stacks, cropped to study area
#' @export
#' @examples
#' \dontrun{
#' cropped_landsat_list <- crop_landsat_list(landsat_dir)
#' }
crop_landsat_list <- function(landsat_dir) {
  # Avoid "no visible binding for global variable" NOTE
  band_L89 <- band_L457 <- aoi <- BOA_dir <- NULL

  landsat_dir_list <- grep("L*02*", list.dirs(landsat_dir), value = TRUE)
  cropped_list <- lapply(landsat_dir_list, function(s) {
    mtl_file <- list.files(s, pattern = "MTL.*xml$",
                           recursive = TRUE, full.names = TRUE, )[1]
    if (! file.exists(mtl_file)) {
      message("No metadata file in landsat dir: ", s, "Skipping...")
      return(NULL)
    }
    mtl <- xml2::read_xml(mtl_file)
    # Values for gain and offset are read from the XML metadata
    # https://www.usgs.gov/faqs/how-do-i-use-a-scale-factor-landsat-level-2-science-products
    gain <- xml2::xml_text(
      xml2::xml_find_first(mtl, ".//REFLECTANCE_MULT_BAND_1"))
    offset <- xml2::xml_text(
      xml2::xml_find_first(mtl, ".//REFLECTANCE_ADD_BAND_1"))
    gain <- as.numeric(gain)
    offset <- as.numeric(offset)

    # Read in tifs
    if (grepl("LC08", s) | grepl("LC09", s)) {
      band_ids <- band_L89
    }
    if (grepl("LE07", s) | grepl("LT05", s) | grepl("LT04", s)) {
      band_ids <- band_L457
    }
    img_list <- lapply(band_ids, function(b){
      # img_path - full names in only one landsat folder
      # filter only sr bands
      img_path <- dir(s)[grepl(pattern = "*_SR_B[0-9]*.TIF$", x = dir(s))]
      img_path <- img_path[grepl(pattern = b, img_path, fixed = TRUE)]
      img_path <- file.path(s, img_path)
      # Use the \code{win} parameter of \code{terra::rast} to crop Landsat tile to aoi
      rst <- terra::rast(img_path, win = terra::ext(aoi))
      return(rst)
    })

    img_stk <- terra::rast(img_list)
    # All bands use the same gain, offset
    img_stk <- img_stk*gain + offset

    # Save to BOA dir
    # Create filename; Prepare file name parts for saving rasters
    # the name is formed from folder
    s_parts <- unlist(strsplit(basename(s), "_"))
    BOA_file <- paste(s_parts[1], s_parts[2], s_parts[3],
                      s_parts[4], s_parts[6], "BOA.tif", sep = "_")
    terra::writeRaster(img_stk,
                       file.path(BOA_dir, BOA_file), overwrite = TRUE)
    return(img_stk)
  }) # end of cropped_list
  return(cropped_list)
}
