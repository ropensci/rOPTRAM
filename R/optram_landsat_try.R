#' @title Handle Satellite imagery in original landsat format
#' @description
#' Use this function to prepares vegetation index and
#' SWIR Transformed Reflectance (STR) rasters
#' when you have already downloaded landsat image files in advance
#' THis function assumes that atmospheric correction has been applied.
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
#' @examples

#' @param landsat_dir, string, full path to containing folder of downloaded (unzipped)
#landsat_dir = "C:/Users/Natalya/Downloads/landsat_oPTRAM"

#' @param aoi_file, string, path to boundary polygon spatial file of area of interest
#???????????????????????????????????????????????????
# What is correct: only path, or path with the name?
# 1. aoi_file <- "D:/rOPTRAM/aoi" # aoi_name = "aoi"
# 2. aoi_file <- "D:/rOPTRAM/aoi/migda_perimeter.shp" # aoi_name = "migdaperimeter"

#' @param vi, string, which VI to prepare, either 'NVDI' (default) or 'SAVI' or 'MSAVI'
#vi = "NDVI"

#' @param LC_output_dir, string, directory to save the derived products,
#'      defaults to tempdir()
#LC_output_dir = "D:/rOPTRAM/derived_products"

#' @param data_output_dir, string, path to save coeffs_file
#'      and STR-VI data.frame, default is tempdir()
#data_output_dir = "D:/rOPTRAM/output"

#' @return coeffs, list, the derived trapezoid coefficients
# In the following FUN call all data is saved in tmp folders
# optram_landsat <- function(landsat_dir,
#                       aoi_file,
#                        vi = 'NDVI',
#                        LC_output_dir = tempdir(),
#                        data_output_dir = tempdir()) {

  optram_landsat <- function(landsat_dir,
                             aoi_file,
                             vi = 'NDVI',
                             LC_output_dir,
                             data_output_dir) {

    # Avoid "no visible binding for global variable" NOTE
    landsat_list <- band_ids <- aoi <- derived_rasters <- xml_file <- NULL
    img_nodes <- img_paths <- img_path <- mtd_file <- mtd <- epsg_code <- NULL
    datestr <- VI_STR_list <- stk <- VI_df <- VI_idx <- NULL
    STR <- STR_df <- full_df <- NULL

    # Loop over the downloaded LC folders (dates),
    # create NDVI and STR indices for each and crop to aoi

# TODO: How to recognize folder of Landsat imagery? Landsat_list - folders with images
# MS:
# The list.dirs() function takes a 'pattern=' argument. So above two lines can be:
#    landsat_list <- list.dirs(landsat_dir,
#                            pattern = "L.*_02_T.*TIF$",
#                           full.names = TRUE, recursive = TRUE)
# N:
# this is working:
    landsat_list <- list.dirs(landsat_dir)[grepl("L*_02_T1",list.dirs(landsat_dir))]

# https://www.usgs.gov/faqs/how-do-i-use-a-scale-factor-landsat-level-2-science-products
# There is extraction from metadata down

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
    # Get Area of interest
    #?????????????????????????? Warning message:
    #                           [vect] Z coordinates ignored
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

# TODO: How to get list of image bands from metadata
    # derived_rasters FUN uses landsat images folders. Output is derived_rasters
    #s <- landsat_list # only to check this FUN
    # s=s[1] # for test only

    derived_rasters <- lapply(landsat_list, function(s) {
        # Get CRS for this landsat dataset, and reproject AOI
        mtl_file <- list.files(s, pattern = "MTL.*xml$",
                                recursive = TRUE, full.names = TRUE, )[1]
        if (! file.exists(mtl_file)) {
            warning("No metadata file in landsat dir: ", s, "Skipping...")
            return(NULL)
        }
        mtl <- xml2::read_xml(mtl_file)
        # epsg will be taken from the first image.
        # take gain and offset.
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

#MS: Are you going to use static values for gain and offset?
# or read from the XML metadata?
#N: values for gain and offset are read from the XML metadata
          gain <- xml2::xml_text(xml2::xml_find_first(mtl, ".//REFLECTANCE_MULT_BAND_1"))
          offset <- xml2::xml_text(xml2::xml_find_first(mtl, ".//REFLECTANCE_ADD_BAND_1"))
          gain <- as.numeric(gain)
          offset <- as.numeric(offset)

# TODO: what file extension of original Landsat imagery
#         img_file <- paste0(img_path, ".jp2")
          img_path <- file.path(s, img_path)
          rst <- terra::rast(img_path, win = terra::ext(aoi))
          return(rst)
        })  # end-of-img_list is working. the result is img_list

        img_stk <- terra::rast(img_list)
        img_stk <- img_stk*gain + offset

        # Save to BOA dir
        # Create filename
        # Prepare file name parts for saving rasters
        # the name is formed from folder
        s_parts <- unlist(strsplit(basename(s), "_"))
        BOA_file <- paste(s_parts[1], s_parts[2],s_parts[3],s_parts[4],s_parts[6],
                        aoi_name, "BOA.tif", sep = "_")
        terra::writeRaster(img_stk,
                         file.path(BOA_dir, BOA_file), overwrite = TRUE)
        return(img_stk)
    }) # enf-of-derived_rasters

    # Get VI and STR from this list of raster stacks
    VI_STR_list <- lapply(1:length(derived_rasters), function(x) {
# LANDSAT: Each item in the derived_rasters list is a raster stack, with 3 bands:
# R, NIR, SWIR 2200
      stk <- derived_rasters[[x]]
      if (is.null(stk)) {
        return(NULL)
      }
      # Use the metadata file from landsat directory name to get image date
      s <- landsat_list[x]
      # Prepare file name parts for saving rasters
#      s_parts <- unlist(strsplit(basename(s), "_"))
#     mtd_file <- list.files(s, pattern = "MTD_TL.*xml$",
#                            recursive = TRUE, full.names = TRUE, )[1]
#      if (! file.exists(mtd_file)) {
#        warning("No metadata file in landsat dir: ", s, "Skipping...")
#       return(NULL)
#      }
#      mtd <- xml2::read_xml(mtd_file)
#     datestr <- as.Date(xml2::xml_text(xml2::xml_find_first(mtd, ".//SENSING_TIME")))
#      #datetime <- strptime(datestr, format = "%FT%X", tz = "UTC")


      mtl_file <- list.files(s, pattern = "MTL.*xml$",
                             recursive = TRUE, full.names = TRUE, )[1]
      if (! file.exists(mtl_file)) {
        warning("No metadata file in landsat dir: ", s, "Skipping...")
        return(NULL)
      }
#extract time, date and create datetime
        mtl <- xml2::read_xml(mtl_file)
#        timestr <- xml2::xml_text(xml2::xml_find_first(mtl, ".//SCENE_CENTER_TIME"))
        datestr <- as.Date(xml2::xml_text(xml2::xml_find_first(mtl, ".//DATE_ACQUIRED")))
#        datestr <- xml2::xml_text(xml2::xml_find_first(mtl, ".//DATE_ACQUIRED"))
#        datestr1 <- paste0(datestr, "T",timestr) # datestr1 =  "2022-06-11T08:11:14.1035130Z"
#        datetime <- strptime(datestr1, format = "%FT%X", tz = "UTC")
#        datetime = "2022-06-11 08:11:14 UTC"


      VI_idx <- rOPTRAM::calculate_vi(stk, vi, redband = 1, nirband = 2)
      VI_df <- terra::as.data.frame(VI_idx, xy = TRUE)
      # Add image date to dataframe
      VI_df['Date'] <- datestr

#      STR <- rOPTRAM::calculate_str(stk, swirband = 5)
      STR <- rOPTRAM::calculate_str(stk, swirband = 3)
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
