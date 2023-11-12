#' @title Acquire Sentinel 2 images at a given location and date range
#' @description Use the `sen2r` package to acquire, preprocess and crop
#'  Sentinel 2 satellite imagery.
#'  L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020)
#'  sen2r: An R toolbox for automatically downloading and preprocessing
#'  Sentinel-2 satellite data
#'  Computers & Geosciences, 139, 104473
#'  doi: 10.1016/j.cageo.2020.104473
#'  URL: https://sen2r.ranghetti.info/.
#'  The package prepares vegetation indicies, cropped to an area of interest.
#' @param aoi_file, string, full path to polygon spatial file of
#'      boundary of area of interest
#' @param from_date, string, represents start of date range,
#'      formatted as "YYYY-MM-DD"
#' @param to_date, string, end of date range, formatted as "YYYY-MM-DD"
#' @param max_cloud, integer, maximum percent of cloud cover. Default 15.
#' @param scihub_user, string, username on Copernicus scihub
#' @param scihub_pass, string, password for access to Copernicus scihub
#' @param timeperiod, string, either "full" for the whole date range,
#' or "seasonal" for only months specified, but over the full date range.
#' @param output_dir, string, path to save downloaded, and processed imagery
#' @param veg_index, string, which index to prepare. Default "NDVI".
#'  Can be "NDVI", "SAVI", "MSAVI", etc
#' @param remove_safe, string, "yes" or "no":
#'      whether to delete downloaded SAFE directories
#'      after processing, default "yes"
#' @return output_path, string, path to downloaded files
#' @export
#' @note
#' Access to Copernicus Sentinel Hub requires registration. If you have already
#' registered, and saved your credentials into the default file,
#' (typically: ~/.sen2r/apihub.txt)
#' then you can leave the scihub_user and scihub_pass empty.
#' If your credentials are **not** yet stored, then
#' - register on the Scihub website:
#' - https://scihub.copernicus.eu/userguide/SelfRegistration
#' - enter your user and pass parameters in this function call,
#'   and they will be stored into the default location.
#'
#' This function calls `sen2r()` from the {sen2r} package. This function
#' acquires Sentinel 2 imagery, clips to aoi,
#' and prepares multiband output rasters, save to the `output_dir`.
#' Only L2A (atmospherically corrected) images are acquired, and therefore:
#'  - only one NIR band, B08 is used. The lower res B8A is not used
#'  - furthermore, the cirrus band B09 is not relevant for BOA level
#'  - so band 10 is the SWIR refelctance at 1600 nm,
#'    and band 11 is reflectance at 2200 nm.
#'
#'
#' @examples
#' \dontrun{
#' from_date <- "2018-12-01"
#' to_date <- "2019-04-30"
#' aoi <- system.file("extdata", "migda_aoi.gpkg", package = 'rOPTRAM')
#' s2_file_list <- optram_acquire_s2(aoi,
#'                                  from_date, to_date,
#'                                  scihub_user = "userxxx",
#'                                  scihub_pass = "secretxyz"
#'                                  timeperiod = "full",
#'                                  veg_index = "SAVI")
#' }

optram_acquire_s2 <- function(
      aoi_file,
      from_date, to_date,
      scihub_user = NULL, scihub_pass = NULL,
      max_cloud = 10,
      timeperiod = "full",
      output_dir = tempdir(),
      remove_safe = "yes",
      veg_index = "NDVI") {
  # Avoid "no visible binding for global variable" NOTE
  sen2r_version <- NULL
  scihub_ok  <- gcloud_ok <- FALSE
  # Download Sentinel 2 images during the requested date range,
  # and clip to the area of interest
  # Pre flight checks...
  if (!check_aoi(aoi_file)) {
    return(NULL)
  }

  # Scihub API credentials?
  scihub_ok <- check_scihub_access(scihub_user, scihub_pass)
  # Where is 'gsutil' installed?
  if (Sys.info()['sysname'] == 'Windows') {
    # Assume that gcloud-sdk is installed in USER's home dir
    # Get the first instance of gsutil file
    homedir <- Sys.getenv("USERPROFILE")
    gsutil_path <- system2("WHERE",
                      paste("/R", homedir, "gsutil"),
                      stdout = TRUE)[1]
  } else {
    gsutil_path <- Sys.which("gsutil")
  }

  gcloud_ok <- sen2r::check_gcloud(gsutil_path, check_creds = FALSE)
  if (gcloud_id) {
    message("Using gcloud")
    servers <- ("gcloud")
  } else {
    warning("No access to Sentinel or Google cloud",
            "\nExiting")
    return(NULL)
  }

  # Disable scihub altogether, since Copernicus DataSpace, 1/11/2023
  # if (scihub_ok && gcloud_ok) {
  #   servers <- c("scihub", "gcloud")
  # } else if (scihub_ok) {
  #   message("Using only Sentinel scihub")
  #   servers <- c("scihub")
  # } else if (gcloud_ok) {
  #   message("Using only gcloud")
  #   servers <- c("gcloud")
  # } else {
  #   warning("No access to Sentinel or Google cloud",
  #           "\nExiting")
  #   return(NULL)
  # }

  # Avoid "no visible binding for global variable" NOTE
  aoi_name  <- result_list <- NULL
  # Checks OK, proceed to download
  aoi_name <- rOPTRAM::aoi_to_name(aoi_file)
  # Make sure output_dir exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Get username and pass from Env variables (also on gitlab)
  sen2r::write_scihub_login(username = Sys.getenv("SCIHUB_USER"),
                            password = Sys.getenv("SCIHUB_PASS"))
  #config_file <- system.file("extdata", "s2_config.json", package = "rOPTRAM")
  #sen2r_version <- utils::packageVersion("sen2r")
  result_list <- sen2r::sen2r(
      #param_list = config_file,
      gui = FALSE,
      server = servers,
      rm_safe = remove_safe,
      max_cloud_safe = max_cloud * 1.5,
      max_mask = max_cloud,
      timewindow = c(from_date, to_date),
      timeperiod = timeperiod,
      list_prods = c("BOA"),
      # in rOPTRAM: "veg_index" (single character string)
      list_indices = c(veg_index),
      resampling = "bilinear",
      extent = aoi_file,
      extent_name = aoi_name,
      extent_as_mask = TRUE,
      path_l2a = output_dir,
      path_out = output_dir,
      path_indices = output_dir,
      thumbnails = FALSE,
      preprocess =  TRUE,
      s2_levels = "l2a",
      sel_sensor = c("s2a", "s2b"),
      online = TRUE,
      order_lta = TRUE,
      downloader = "builtin",
      overwrite_safe = FALSE,
      step_atmcorr = "l2a",
      sen2cor_use_dem = FALSE,
      sen2cor_gipp = NULL,
      s2orbits_selected = NULL,
      list_rgb = NULL,
      rgb_ranges = NULL,
      index_source = "BOA",
      mask_type = "cloud_and_shadow",
      mask_smooth = 0,
      mask_buffer = 0,
      clip_on_extent = TRUE,
      reference_path = NULL,
      res = NULL,
      res_s2 = "10m",
      unit = "Meter",
      proj = NULL,
      resampling_scl = "near",
      outformat = "GTiff",
      rgb_outformat = "GTiff",
      index_datatype = "Int16",
      compression = "DEFLATE",
      rgb_compression = "DEFLATE",
      overwrite = FALSE,
      path_l1c = NULL,
      path_tiles = NULL,
      path_merged = NULL,
      path_rgb = "",
      path_subdirs = TRUE,
      log = c(NA,NA),
      parallel = TRUE,
      processing_order = "by_groups",
    )
  return(result_list)
}
