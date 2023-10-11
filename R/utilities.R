#' @title Check access to Sentinel Hub, using sen2r
#' 
#' @description
#' Verify that sen2r is installed,
#' and check credentials for access to Sentinel Hub
#' @param scihub_user, string, scihub username
#' @param scihub_pass, string, scihub password
#' @export
#' @return boolean, whether access to scihub using sen2r is possible

check_scihub_access <- function(scihub_user = NULL,
                                scihub_pass = NULL) {
    # Avoid "no visible binding for global variable" NOTE
    is_online <- site <- NULL
    # First check for internet connection
    is_online <- function(site = "https://scihub.copernicus.eu/") {
        tryCatch({
            readLines(site, n = 1)
            TRUE
        },
        # warning = function(w) invokeRestart("muffleWarning"),
        error = function(e) FALSE)
    }

    if (!is_online()) {
        message("No internet connection. Downloading data is not currently possible")
        return(FALSE)
    }

    # Is sen2r installed?
    if (system.file(package='sen2r') == "") {
        message("This function requires the `sen2r` package.", "\n",
        "Please install that package first before running function")
        return(FALSE)
    }
    # Check for stored credentials (by default in "~/.sen2r/apihub.txt")
    if (sen2r::is_scihub_configured()) {
        return(TRUE)
    } else {
        if (!is.null(scihub_user) && !is.null(scihub_pass)) {
            # If credentials have never preciously been been stored,
            # check the user supplied values, to be sure they are valid.
            if(sen2r::check_scihub_login(scihub_user, scihub_pass)) {
                sen2r_path <- file.path(path.expand("~"), ".sen2r")
                if (!dir.exists(sen2r_path)) {
                    dir.create(sen2r_path)
                }
                # store valid credentials into "apihub.txt" for future
                apihub_path <- file.path(sen2r_path, "apihub.txt")
                sen2r::write_scihub_login(scihub_user, scihub_pass, apihub_path = apihub_path)
                return(TRUE)
            } else {
                message("Login credentials not accepted by ESA Sentinel Hub",
                "\n", "Please verify Sentinel Hub login credentials.")
                return(FALSE)
            }
        } else {
            # No stored credentials, and no values passed in by user.
            # Print message and exit.
            message("ESA Sentinel Hub requires authentication", "\n",
                    "Please register at:", "\n",
                    "https://scihub.copernicus.eu/userguide/SelfRegistration",
                    "then rerun function specifying both:", "\n",
                    "scihub_user, and scihub_pass")
            return(FALSE)
        }
    }
}

#' @title Check aoi_file
#' @description
#' Check that aoi file exists, and is a spatial file
#' @param aoi_file, string
#' @export
#' @return boolean, TRUE when file exists, and is spatila
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
#' @param img_stk, terra SpatRaster, multiband stack of images, already clipped to aoi
#' @param redband, integer, number of red band
#' @param nirband, integer, number of NIR band
#' @param viname, string, which VI to prepare, either 'NDVI' (default) or 'SAVI' or 'MSAVI'
#' @export
#' @return vi_rast, SpatRaster of vegetation index
#' @examples
#' img_stk <- terra::rast(system.file("extdata",
#'          "BOA",
#'          "S2A2A_20230301_121_migdaaoi_BOA_10.tif",
#'          package = "rOPTRAM"))
#' vi <- calculate_vi(img_stk)

calculate_vi <- function(img_stk, viname = "NDVI", redband = 3, nirband = 4) {
    # Avoid "no visible binding for global variable" NOTE
    nir <- red <- vi_rast <- NULL

    nir <- img_stk[[nirband]]
    red <- img_stk[[redband]]
    if (viname == "NDVI") {
        vi_rast <- ((nir - red) / (nir + red))
    } else if (viname == "SAVI") {
        vi_rast <- ((1.5 * (nir - red)) / (nir + red + 0.5) )
    } else if (viname == "MSAVI") {
        vi_rast <- ((2 * nir + 1 - sqrt((2 * nir + 1)^2 - 8 * (nir - red))) / 2)
    } else {
        warning("Unrecognized index: ", viname)
        vi_rast <- NULL
        return(NULL)
    }
    names(vi_rast) <- viname
    return(vi_rast)
}


#' @title Calculate STR from SWIR band of bottom of atmosphere images
#'
#' @description
#' Use this function to prepare STR from SAFE imagery
#' when you have already downloaded Sentinel 2 image files in advance
#' (without using `sen2r`).
#'
#' @param img_stk, terra SpatRaster, multiband stack of images, already clipped to aoi
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
  # STR = (1−SWIR)^2 / 2*SWIR
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


#' @title Get name string for AIO from the full file name
#' @description
#' Extract a string from the full path to AIO file
#' @param aoi_file, string, full path to AOI file
#' @export 
#' @return aoi_name, string

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
