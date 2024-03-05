#' @title Check Area of Interest File
#' @description
#' Check that aoi file exists, and is a spatial file
#' @param aoi_file, string
#' @return boolean, TRUE when file exists, and is spatial
#' @noRd
#' @examples
#' aoi_file <- system.file("extdata", "lachish.gpkg")
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

#' @title Check Date Format
#' @description
#' Check that date string is formatted correctly "YYYY-MM-DD"
#' @param date_string, string
#' @return boolean, TRUE when formatted correctly
#' @noRd
#' @examples
#' (date_ok <- check_date_string("2023-01-31")) # returns TRUE
#' (date_ok <- check_date_string("2023-02-30")) # returns FALSE

check_date_string <- function(from_string, to_string) {
  if (!is.character(from_string) |
      !is.character(to_string)) return(FALSE)
  fryr <- unlist(strsplit(x = from_string, split = "-", fixed = TRUE))[1]
  toyr <- unlist(strsplit(x = to_string, split = "-", fixed = TRUE))[1]
  if (nchar(fryr) != 4 | nchar(toyr) != 4) return(FALSE)
  frd <- as.Date(from_string, format = "%Y-%m-%d")
  tod <- as.Date(to_string, format = "%Y-%m-%d")
  if (is.na(frd) | is.na(tod)) return(FALSE)
  if (tod <= frd) return(FALSE)
  return(TRUE)
}


#' @title Check SWIR band values
#' @description
#' Check that SWIR_band is specified as one of (11, 12)
#' @param SWIR_band, numeric
#' @return boolean, TRUE when value is OK
#' @noRd
#' @examples
#' check_swir_band(11)         # TRUE
#' check_swir_band(10)         # FALSE
#' check_swir_band(c(11, 12))  # FALSE
check_swir_band <- function(SWIR_band) {
  if (! is.numeric(SWIR_band)) {
    message("SWIR band: ", SWIR_band, "
            not correct. Please specify an integer")
    return(FALSE)
  } else if (any(length(SWIR_band) > 1 | SWIR_band < 11 | SWIR_band > 12)) {
    message("SWIR band: ", SWIR_band, "
            not correct. Please specify either 11, or 12")
    return(FALSE)
  }
  return(TRUE)
}



#' @title Calculate Vegetation Index from Bottom of Atmosphere Image Bands
#' @description
#' Use this function to prepare vegetation index from SAFE imagery
#' when you have already downloaded Sentinel 2 image files in advance
#' @param img_stk, terra SpatRaster, multiband stack of images,
#'            already clipped to aoi
#' @param redband, integer, number of red band
#' @param greenband, integer, number of green band
#' @param blueband, integer, number of blue band
#' @param nirband, integer, number of NIR band
#' @param viname, string, which VI to prepare,
#'            either 'NDVI' (default) or 'SAVI' or 'MSAVI'
#' @param scale_factor, numeric, scales Sentinel-2 15 bit values
#'            down to range (0,255) as expected for vegetation indices
#'            default 32768 (2^15)
#' @note The scale_factor parameter reduces numeric range "digital number",
#'  (DN) of Sentinel-2 images from 15 bit integer to 8 bit.
#'  Vegetation indices such as SAVI expect values in the range (0, 255).
#'  This scale_factor produces values in that range.
#'  When using Landsat 8 images (DN already in range of (0, 255)) then
#'  set scale_factor to 255.
#' @export
#' @return vi_rast, SpatRaster of vegetation index
#' @examples
#' img_stk <- terra::rast(system.file("extdata", "BOA",
#'          "BOA_2022-12-11.tif", package = "rOPTRAM"))
#' vi <- calculate_vi(img_stk)

calculate_vi <- function(img_stk, viname = "NDVI",
                         redband = 4,
                         greenband = 3,
                         blueband = 2,
                         nirband = 5,
                         scale_factor = 2^15) {
    # Avoid "no visible binding for global variable" NOTE
    nir <- red <- blue <- green <- vi_rast <- NULL
    if (terra::nlyr(img_stk) < 12) {
      message("BOA image stack does not contain all bands")
      return(NULL)
    }
    nir <- img_stk[[nirband]]
    red <- img_stk[[redband]]
    blue <- img_stk[[blueband]]
    green <- img_stk[[greenband]]
    # Rescaling
    nir <- 255 * (nir - min(terra::values(nir))) / scale_factor
    red <- 255 * (red - min(terra::values(red))) / scale_factor
    blue <- 255 * (blue - min(terra::values(blue))) / scale_factor
    green <- 255 * (green - min(terra::values(green))) / scale_factor

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


#' @title Calculate STR from SWIR Bottom of Atmosphere Band
#' @description
#' Use this function to prepare STR from SAFE imagery
#' when you have already downloaded Sentinel 2 image files in advance
#' @param img_stk, terra SpatRaster, multiband stack of images,
#'          already clipped to aoi
#' @param SWIR_band, integer, number of SWIR band, one of (11 or 12)
#' @param scale_factor, integer, scaling factor for EO data source
#'      default 10000, to scale Sentinel-2 15 bit DN to range (0, 1)
#' @export
#' @note For Landsat images, scale_factor should be 1,
#'      since Landsat metadata contains gain and offset
#'      for scaling image bands.
#' @return STR, SpatRaster of STR band
#' @examples
#' img_stk <- terra::rast(system.file("extdata", "BOA",
#'          "BOA_2022-12-11.tif", package = "rOPTRAM"))
#' STR_dir = tempdir()
#' str <- calculate_str(img_stk, SWIR_band = 11)
calculate_str <- function(img_stk,
                          SWIR_band = c(11,12), scale_factor = 10000) {
  # Sadeghi, M., Babaeian, E., Tuller, M., Jones, S.B., 2017.
  # The optical trapezoid model:
  # A novel approach to remote sensing of soil moisture
  # applied to Sentinel-2 and Landsat-8 observations.
  # Remote Sensing of Environment 198, 52–68.
  # https://doi.org/10.1016/j.rse.2017.05.041
  #
  # STR = (1-SWIR)^2 / 2*SWIR
  #
  # Make sure SWIR_band is one of 11 or 12
  if (!check_swir_band(SWIR_band)) return(NULL)
  if (terra::nlyr(img_stk) < 12) {
    message("BOA image stack does not contain all bands")
    return(NULL)
  }
  SWIR_DN <-  img_stk[[SWIR_band]]
  # back to native scale
  SWIR <-  SWIR_DN / scale_factor
  # Convert from Solar irradiance
  # solar_irradiance_12 <- 87.25
  # SWIR <- (SWIR_irr/10) * solar_irradiance_12
  STR <- (1 - SWIR)^2 / (2*SWIR)
  names(STR) <- "STR"
  return(STR)
}


#' @title Get Name String for Area of Interest from Full File Name
#' @description
#' Extract a string from the full path to area of interest (AOI) file
#' @param aoi_file, string, full path to AOI file
#' @noRd
#' @keywords Internal
#' @return aoi_name, string
#' @examples
#' aoi_file <- system.file("extdata", "lachish.gpkg")
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

#' @title Store CDSE Client Credentials
#' @description Store CDSE clientid and secret into a file
#' The file location is system specific.
#' Users who chose to save CDSE credentials can use this function
#' (and then the `retrieve_cdse_credentials()` afterwards)
#' The clientid and secret are obtained from:
#'
#' @param clientid, string, user's OAuth client id
#' @param secret, string, user's OAuth secret
#' @return NULL
#' @note
#' Both clientid and secret can alternatively be supplied as
#' environment variables: OAUTH_CLIENTID and OAUTH_SECRET.
#' If these env variables are available (and no values are entered
#' as function arguments) they will be used to store credentials.
#' @export

store_cdse_credentials <- function (clientid = NULL,
                                    secret = NULL) {
  store_creds <- function(clientid, secret, creds_file) {
    creds <- data.frame("clientid" = clientid, "secret" = secret)
    jsonlite::write_json(creds, creds_file)
    message("Credentials are saved to:", creds_file)
  }
  switch(Sys.info()['sysname'],
         "Windows" = {creds_path <- file.path(Sys.getenv("LOCALAPPDATA"),
                                              "CDSE")},
         "Linux" = {creds_path <- file.path(Sys.getenv("HOME"), ".CDSE")},
         "macOS" = {creds_path <- file.path(Sys.getenv("HOME"),
                              "Library", "Preferences", "CDSE")},
         {message("Platform is not identified. No credentials are saved")}
  )
  if (!dir.exists(creds_path)) dir.create(creds_path)
  creds_file <- file.path(creds_path, "cdse_credentials.json")
  if (!is.null(clientid) & !is.null(secret)) {
    store_creds(clientid, secret, creds_file)
  } else {
    # Allow for retrieving OAuth credentials from env variables:
    clientid <- Sys.getenv("OAUTH_CLIENTID")
    secret <- Sys.getenv("OAUTH_SECRET")
    if (clientid != '' & secret != '') {
      store_creds(clientid, secret, creds_file)
  } else {
    message("No clientid or secret supplied. No credentials are saved")
    return(NULL)
  }
}

#' @title Retrieve CDSE Client Credentials from File
#' @description Retrieve CDSE clientid and secret from file
#' The file location is system specific. It would have been setup
#' in advance using the `store_cdse_credentials()` function
#' @return A data frame containing the retrieved CDSE clientid and secret,
#' or NULL if credentials are not available.
#' @export

retrieve_cdse_credentials <- function() {
  creds_path <- switch(Sys.info()['sysname'],
         "Windows" = {file.path(Sys.getenv("LOCALAPPDATA"), "CDSE")},
         "Linux" = {file.path(Sys.getenv("HOME"), ".CDSE")},
         "macOS" = {file.path(Sys.getenv("HOME"),
                              "Library", "Preferences", ".CDSE")},
         {message("Platform is not identified. No credentials are saved")}
  )
  if (!dir.exists(creds_path)) {
    warning("No credentials directory:", creds_path,
            " found. Credentials not available")
    return(NULL)
  }
  creds_file <- file.path(creds_path, "cdse_credentials.json")
  if (!file.exists(creds_file)) {
    warning("No credentials file:", creds_file,
            " found. Credentials not available.")
    return(NULL)
  }
  creds <- jsonlite::read_json(creds_file)
  return(creds)
}
