#' @title Display or Set Package Options
#' @description {rOPTRAM} uses several package options.
#' This function displays the current defined options,
#' (showing the default options when the package is first loaded),
#' and allows users to set new values for each option.
#' @param opt_name, string, one of the package options.
#' Default NULL
#' @param opt_value, string, numeric, or boolean, depending on opt_name.
#' @param show_opts, boolean, default TRUE, prints a list of all optram options
#' Default NULL
#' @return NULL
#' @export
#'
#' @examples
#' opts <- options()
#' optram_options()      # prints out list of current options
#' optram_options("SWIR_band", 12)
#' optram_options("veg_index", "SAVI")
#' optram_options("trapezoid_list", "exp")  # fails
#' options(opts)
#'
#' @note
#' When no new option name or value is specified,
#' a list of currently defined options is printed.
#'
#' {rOPTRAM} defines the following options at startup
#'\tabular{lll}{
#'  opt_name     \tab default  \tab other possible values \cr
#'  ---------------- \tab---------------\tab ------------------- \cr
#'  veg_index    \tab "NDVI"  \tab "SAVI", "MSAVI" \cr
#'  remote       \tab "scihub"   \tab "openeo" \cr
#'  period       \tab "full"   \tab "seasonal" \cr
#'  max_cloud    \tab 12 \tab between 0 and 100 \cr
#'  vi_step      \tab 0.005  \tab usually between 0.01 and 0.001 \cr
#'  trapezoid_method \tab "linear"  \tab "polynomial" or "exponential" \cr
#'  SWIR_band    \tab 11    \tab 12 \cr
#'  max_tbl_size \tab 1e+6  \tab depends on computer resources \cr
#'  rm.low.vi    \tab FALSE \tab TRUE \cr
#'  rm.hi.str    \tab FALSE \tab TRUE \cr
#'  plot_colors  \tab "no"  \tab "no" = uniform green color for all points \cr
#'   \tab  \tab "features" = points colored by aoi features \cr
#'   \tab  \tab "density" = points colored by point density \cr
#'   \tab  \tab "contours" = plots density contour lines \cr
#'   \tab  \tab "months" = points colored by month of image date \cr
#'  feature_col  \tab "ID" \tab string, name of *numeric* column \cr
#'   \tab  \tab that contains feature ID's for coloring plot \cr
#'  edge_points  \tab TRUE  \tab FALSE, whether to add \cr
#'   \tab  \tab the trapezoid edge points to the plot \cr
#'  overwrite    \tab  FALSE \tab Set to TRUE to re-download already acquired S2 images \cr
#'}
optram_options <- function(opt_name = NULL, opt_value=NULL,
                           show_opts = TRUE) {
  # Internal functions
  display_optram_options <- function() {
    opts <- options()
    optram_opts <- opts[grep(pattern = "optram", x=names(opts),
                             fixed = TRUE)]
    for (i in 1:length(optram_opts)) {
        short_name <- sub(pattern = "optram.",
                          replacement = "",
                          x = names(optram_opts)[i])
        opt_1 <- paste(short_name, "=", optram_opts[i][[1]])
        print(opt_1)}
  }

  add_new_option <- function(opt_name, opt_val) {
    new_opt <- list(opt_val)
    names(new_opt) <- paste0("optram.", opt_name)
    options(new_opt)
    return(paste("New option for", opt_name, "applied."))
  }

  if (any(is.null(opt_name) | is.null(opt_value))) {
    # No new option, so just display current options
    display_optram_options()
    return(NULL)
  }

  # Reset option
  opt_names <- c("veg_index","remote", "period", "max_cloud", "vi_step",
                 "trapezoid_method", "SWIR_band", "max_tbl_size",
                 "rm.low.vi", "rm.hi.str", "plot_colors", "feature_col",
                 "edge_points", "overwrite")
  if (opt_name %in% opt_names) {
    # Setup conditions for each option name
    cond_func <- switch(opt_name,
              "veg_index" = function(opt_value) {
                return(opt_value %in% c("NDVI", "SAVI", "MSAVI", "CI", "BSCI"))},
              "remote" = function(opt_value) {
                return(opt_value %in% c("scihub", "openeo"))
              },
              "period" = function(opt_value) {
                return(opt_value %in% c("full", "seasonal"))
              },
              "max_cloud" = function(opt_value) {
                return(is.numeric(opt_value) & opt_value <=100 & opt_value >=0)
              },
              "vi_step" = function(opt_value) {
                return(is.numeric(opt_value) & opt_value <= 0.02)
              },
              "trapezoid_method" = function(opt_value) {
                return(opt_value %in% c("linear", "exponential", "polynomial"))
              },
              "SWIR_band" = function(opt_value) {
                return(is.numeric(opt_value) &
                         opt_value >= 11 & opt_value <= 12)
              },
              "max_tbl_size" = function(opt_value) {
                return(is.numeric(opt_value) & opt_value > 1e+4)
              },
              "rm.low.vi" = function(opt_value) {
                return(is.logical(opt_value))
              },
              "rm.hi.str" = function(opt_value) {
                return(is.logical(opt_value))
              },
              "edge_points" = function(opt_value) {
                return(is.logical(opt_value))
              },
              "plot_colors" = function(opt_value) {
                return(opt_value %in% c("no", "none", "density",
                                        "feature", "features",
                                        "contour", "contours",
                                        "month", "months"))
              },
              "feature_col" = function(opt_value) {
                return(is.character(opt_value))
              },
              "overwrite" = function(opt_value) {
                return(is.logical(opt_value))
              }
      )
    msg <- ifelse(cond_func(opt_value),
                  add_new_option(opt_name, opt_value),
                  paste("Incorrect value:", opt_value, "for", opt_name))
    message("\n", msg, "\n")
  if (show_opts) display_optram_options()
  } else {
    message("\nUnknown option name: ", opt_name)
  }
}


#' @title Check Area of Interest File
#' @description
#' Check that aoi is {sf} POLYGON or MULTIPOLYGON
#' @param aoi, an {sf} polygon object
#' @return boolean, TRUE when aoi is spatial
#' @noRd
#' @examples
#' aoi <- sf::st_read(system.file("extdata", "lachish.gpkg"))
#' check_aoi(aoi)

check_aoi <- function(aoi) {
  if (is.null(aoi)) {
      message("An area_of_interest polygon {sf} object is required")
      return(FALSE)
  } else if (!inherits(aoi, "sf")) {
      message("AOI is not a recognized {sf} spatial format")
      return(FALSE)
  } else if (all(sf::st_geometry_type(aoi) == "POLYGON") |
             all(sf::st_geometry_type(aoi) == "MULTIPOLYGON")) {
    return(TRUE)
  } else {
    message("AOI must be a POLYGON or MULTIPOLYGON")
    return(FALSE)
  }
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
#'          "BOA_2023-01-20_T36RXV.tif", package = "rOPTRAM"))
#' vi <- calculate_vi(img_stk)

calculate_vi <- function(img_stk,
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
  viname = getOption("optram.veg_index")
  nir <- img_stk[[nirband]]
  red <- img_stk[[redband]]
  blue <- img_stk[[blueband]]
  green <- img_stk[[greenband]]
  # Rescaling
  nir <- 255 * (nir - min(terra::values(nir), na.rm = TRUE)) / scale_factor
  red <- 255 * (red - min(terra::values(red), na.rm = TRUE)) / scale_factor
  blue <- 255 * (blue - min(terra::values(blue), na.rm = TRUE)) / scale_factor
  green <- 255 * (green - min(terra::values(green), na.rm = TRUE)) / scale_factor

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
#' @param scale_factor, integer, scaling factor for EO data source
#'      default 10000, to scale Sentinel-2 15 bit DN to range (0, 1)
#' @export
#' @note For Landsat images, scale_factor should be 1,
#'      since Landsat metadata contains gain and offset
#'      for scaling image bands.
#' @return STR, SpatRaster of STR band
#' @examples
#' img_stk <- terra::rast(system.file("extdata", "BOA",
#'          "BOA_2022-12-11_T36RXV.tif", package = "rOPTRAM"))
#' STR_dir = tempdir()
#' str <- calculate_str(img_stk)
calculate_str <- function(img_stk,
                          scale_factor = 10000) {
  # Sadeghi, M., Babaeian, E., Tuller, M., Jones, S.B., 2017.
  # The optical trapezoid model:
  # A novel approach to remote sensing of soil moisture
  # applied to Sentinel-2 and Landsat-8 observations.
  # Remote Sensing of Environment 198, 52–68.
  # https://doi.org/10.1016/j.rse.2017.05.041
  #
  # STR = (1-SWIR)^2 / 2*SWIR

  SWIR_band = getOption("optram.SWIR_band")
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


#' @title Store CDSE Client Credentials
#' @description Store CDSE clientid and secret into a file
#' The file location is system specific.
#' Users who chose to save CDSE credentials can use this function
#' (and then the \code{\link[rOPTRAM]{retrieve_cdse_credentials}} afterwards)
#' The clientid and secret are obtained from:
#'
#' @param clientid, string, user's OAuth client id
#' @param secret, string, user's OAuth secret
#' @return NULL
#' @note
#' Both \code{clientid} and \code{secret} can alternatively be supplied as
#' environment variables: OAUTH_CLIENTID and OAUTH_SECRET.
#' If these env variables are available (and no values are entered
#' as function arguments) they will be used to store credentials.
#' @export
#' @examples
#' \dontrun{
#' store_cdse_credentials(clientid="<...enter your client id...>",
#'                       secret = "<...enter your secret...>")
#' }

store_cdse_credentials <- function (clientid = NULL,
                                    secret = NULL) {
  store_creds <- function(clientid, secret, creds_file) {
    creds <- data.frame("clientid" = clientid, "secret" = secret)
    jsonlite::write_json(creds, creds_file)
    message("Credentials are saved to:", creds_file)
  }
  creds_path <- switch(Sys.info()['sysname'],
         "Windows" = file.path(Sys.getenv("LOCALAPPDATA"), "CDSE"),
         "Linux" = file.path(Sys.getenv("HOME"), ".CDSE"),
         "Darwin" = file.path(Sys.getenv("HOME"),
                              "Library", "Preferences", ".CDSE")
         # If nothing matches, switch returns NULL
  )
  if (is.null(creds_path)) {
    warning("Platform is not identified. No credentials are saved")
    return(NULL)
  }
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
}

#' @title Retrieve CDSE Client Credentials from File
#' @description Retrieve CDSE clientid and secret from file
#' The file location is system specific. It would have been setup
#' in advance using the \code{\link[rOPTRAM]{store_cdse_credentials}} function
#' @return A data frame containing the retrieved CDSE clientid and secret,
#' or NULL if credentials are not available.
#' @export

retrieve_cdse_credentials <- function() {
  creds_path <- switch(Sys.info()['sysname'],
         "Windows" = {file.path(Sys.getenv("LOCALAPPDATA"), "CDSE")},
         "Linux" = {file.path(Sys.getenv("HOME"), ".CDSE")},
         "Darwin" = {file.path(Sys.getenv("HOME"),
                              "Library", "Preferences", ".CDSE")}
  )
  if (is.null(creds_path)) {
    warning("Platform is not identified. No credentials are available")
    return(NULL)
  }
  creds_file <- file.path(creds_path, "cdse_credentials.json")
  if (!file.exists(creds_file)) {
    warning("No credentials file:", creds_file,
            " found. Credentials are not available.")
    return(NULL)
  }
  creds <- jsonlite::read_json(creds_file)
  return(creds)
}
