#' @title Check that sen2r is installed
#'
#' @description
#' Verify that sen2r is installed, and site is available.
#' Default download site: "https://scihub.copernicus.eu/"
#' @param site, string, Default is copernicus download site
#' @export
#' @return boolean, whether online and using sen2r is possible
#' @examples
#' site <-  "http://scihub.copernicus.eu"
#' check_scihub_access(site)

check_scihub_access <- function(site = "http://scihub.copernicus.eu") {
    #message("In check_scihub_access:", covr::in_covr())
    # First check for internet connection
    internet_online <- tryCatch({
            readLines(site, n=1)
            TRUE},
        error = {function(e) {
            message("No internet connection to SCIHUB.", "\n",
        "Downloading data is not currently possible")
            FALSE}
    })
    
    if (internet_online) {
        #message("In covr 2:", covr::in_covr())
        # Is sen2r installed?
        if (system.file(package='sen2r') == "") {
            message("This function requires the `sen2r` package.", "\n",
            "Please install that package first before running function")
            return(FALSE)
        }
        # Check sen2r version
        sen2r_version <- utils::packageVersion("sen2r")
        version_ok <- package_version(sen2r_version) > '1.5.0'
        if (!version_ok) {
            message("Version of sen2r pacakge: ", sen2r_version, " is too old. \n",
            "Please update to version > 1.5")
            return(FALSE)
        }
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#' @title Check aoi_file
#' @description
#' Check that aoi file exists, and is a spatial file
#' @param aoi_file, string
#' @export
#' @return boolean, TRUE when file exists, and is spatial
#' @examples
#' aoi_file <- system.file("extdata", "migda_aoi.gpkg")
#' check_aoi(aoi_file)

check_aoi <- function(aoi_file) {
  if (is.null(aoi_file) || !file.exists(aoi_file)) {
      message("An area_of_interest polygon shapefile is required",
      "\n", "Please prepare the area_of_interest boundary file.")
      return(FALSE)
  } else {
    aoi_result <- try(suppressWarnings(sf::st_read(aoi_file)))
    if (inherits(aoi_result, "try-error")) {
        message("Cannot read: ", aoi_file)
        return(FALSE)
    } else if (!inherits(aoi_result, "sf")) {
        message(aoi_file, " is not a recognized spatial format")
        return(FALSE)
    }
  }
    return(TRUE)
}


#' @title Calculate NDVI, SAVI or MSAVI from bottom of atmosphere images
#' @description
#' Use this function to prepare vegetation index from SAFE imagery
#' when you have already downloaded Sentinel 2 image files in advance
#' (without using `sen2r`).
#'
#' @param img_stk, terra SpatRaster, multiband stack of images,
#'            already clipped to aoi
#' @param redband, integer, number of red band
#' @param greenband, integer, number of green band
#' @param blueband, integer, number of blue band
#' @param nirband, integer, number of NIR band
#' @param viname, string, which VI to prepare,
#'            either 'NDVI' (default) or 'SAVI' or 'MSAVI'
#' @export
#' @return vi_rast, SpatRaster of vegetation index
#' @examples
#' img_stk <- terra::rast(system.file("extdata",
#'          "BOA",
#'          "S2A2A_20230301_121_migdaaoi_BOA_10.tif",
#'          package = "rOPTRAM"))
#' vi <- calculate_vi(img_stk)

calculate_vi <- function(img_stk, viname = "NDVI",
                         redband = 3, greenband = 2,
                         blueband = 1, nirband = 4) {
    # Avoid "no visible binding for global variable" NOTE
    nir <- red <- vi_rast <- NULL

    nir <- img_stk[[nirband]]
    red <- img_stk[[redband]]
    blue <- img_stk[[blueband]]
    green <- img_stk[[greenband]]

    if (viname == "NDVI") {
        vi_rast <- ((nir - red) / (nir + red))
    } else if (viname == "SAVI") {
        vi_rast <- ((1.5 * (nir - red)) / (nir + red + 0.5) )
    } else if (viname == "MSAVI") {
        vi_rast <- ((2 * nir + 1 - sqrt((2 * nir + 1)^2 - 
            8 * (nir - red))) / 2)
    } else if (viname == "CI") {
        vi_rast <- (1-((red - blue) / (red + blue)))
    } else if (viname == "BSCI") {
        vi_rast <- ((1-(2*(red - green))) / 
            (terra::mean(green, red, nir, na.rm = TRUE)))
    } else {
        message("Unrecognized index: ", viname)
        vi_rast <- NULL
        return(NULL)
    }
    names(vi_rast) <- "VI"
    return(vi_rast)
}


#' @title Calculate STR from SWIR band of bottom of atmosphere images
#'
#' @description
#' Use this function to prepare STR from SAFE imagery
#' when you have already downloaded Sentinel 2 image files in advance
#' (without using `sen2r`).
#'
#' @param img_stk, terra SpatRaster, multiband stack of images,
#'          already clipped to aoi
#' @param swirband, integer, number of red band
#' @param scale_factor, integer, scaling factor for EO data source
#'      default 10000, to scale Sentinel-2 15 bit DN to range (0, 1)
#' @export
#' @note For Landsat images, scale_factor should be 1,
#'      since Landsat metadata contains gain and offset for scaling image bands.
#' @return STR, SpatRaster of STR band
#' @examples
#' img_stk <- terra::rast(system.file("extdata",
#'          "BOA",
#'          "S2A2A_20230301_121_migdaaoi_BOA_10.tif",
#'          package = "rOPTRAM"))
#' str <- calculate_str(img_stk)


calculate_str <- function(img_stk, swirband = 11, scale_factor = 10000) {
  # Sadeghi, M., Babaeian, E., Tuller, M., Jones, S.B., 2017.
  # The optical trapezoid model:
  # A novel approach to remote sensing of soil moisture
  # applied to Sentinel-2 and Landsat-8 observations.
  # Remote Sensing of Environment 198, 52–68.
  # https://doi.org/10.1016/j.rse.2017.05.041
  #
  # STR = (1-SWIR)^2 / 2*SWIR
  #
    SWIR_DN <-  img_stk[[swirband]]
    # back to native scale
    SWIR <-  SWIR_DN / scale_factor
    # Convert from Solar irradiance
    # solar_irradiance_12 <- 87.25
    # SWIR <- (SWIR_irr/10) * solar_irradiance_12
    STR <- (1 - SWIR)^2 / (2*SWIR)
    names(STR) <- "STR"
    return(STR)
}


#' @title Get name string for Area of Interest from full file name
#' @description
#' Extract a string from the full path to AIO file
#' @param aoi_file, string, full path to AOI file
#' @export
#' @return aoi_name, string
#' @examples
#' aoi_file <- system.file("extdata", "migda_aoi.gpkg")
#' aoi_name <- aoi_to_name(aoi_file)
#' aoi_name

aoi_to_name <- function(aoi_file) {
    aoi_name <- NULL
    if (is.null(aoi_file) || !file.exists(aoi_file)) {
        return(NULL)
    }
    aoi_name <- tools::file_path_sans_ext(basename(aoi_file))
    aoi_name <- gsub(x = aoi_name, pattern = " ", replacement = "")
    aoi_name <- gsub(x = aoi_name, pattern = "\\.", replacement = "")
    aoi_name <- gsub(x = aoi_name, pattern = "_", replacement = "")

    return(aoi_name)
}
