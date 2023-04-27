#' @title Acquire Sentinel 2 images at a given location and date range
#' @description Use the `sen2r` package to acquire, preprocess and crop, Sentinel 2 satellite imagery.
#' L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020)
#' sen2r: An R toolbox for automatically downloading and preprocessing Sentinel-2 satellite data
#' Computers & Geosciences, 139, 104473
#' doi: 10.1016/j.cageo.2020.104473
#' URL: https://sen2r.ranghetti.info/.
#' The package prepares vegetation indicies, cropped to an area of interest.
#' @param aoi_file, string, full path to polygon spatial file of boundary of area of interest
#' @param from_date, string, represents start of date range, formatted as "YYYY-MM-DD"
#' @param to_date, string, end of date range, formatted as "YYYY-MM-DD"
#' @param max_cloud, integer, maximum percent of cloud cover. Default 15.
#' @param scihub_user, string, username on Copernicus scihub
#' @param scihub_pass, string, password for access to Copernicus scihub
#' @param timeperiod, string, either "full" for the whole date range,
#' or "seasonal" for only months specified, but over the full date range.
#' @param output_dir, string, path to save downloaded, and processed imagery
#' @param list_indicies, vector of strings, which indicies to prepare.
#'    Default c("NDVI", "SAVI")
#' @param remove_safe, string, "yes" or "no": whether to delete downloaded SAFE directories
#' after processing, default "yes"
#' @return output_path, string, path to downloaded files
#' @export
#' @note
#' Access to Copernicus Sentinel Hub requires registration. If you have already
#' registered, and saved your credentials into the default "library(sen2r)" file,
#' then you can leave the scihub_user and scihub_pass empty.
#' If your credentials are **not** yet stored, then
#' - register on the Scihub website:
#' - https://scihub.copernicus.eu/userguide/SelfRegistration
#' - enter your user and pass parameters in this function call,
#'   and they will be stored into the default location.
#' @examples
#' \dontrun{
#' from_date <- "2018-12-01"
#' to_date <- "2019-04-30"
#' aoi <- "inst/extdata/migda_aoi.gpkg"
#' s2_file_list <- optram_acquire_s2(aoi,
#'                                  from_date, to_date,
#'                                  scihub_user = "userxxx",
#'                                  scihub_pass = "secretxyz"
#'                                  timeperiod = "full",
#'                                  list_indicies="MSAVI2")
#' }

optram_acquire_s2 <- function(
      aoi_file,
      from_date, to_date,
      scihub_user = NULL, scihub_pass = NULL,
      max_cloud = 10,
      timeperiod = "full",
      output_dir = tempdir(),
      remove_safe = "yes",
      list_indicies = c("NDVI", "SAVI")) {
  # Download Sentinel 2 images during the requested date range,
  # and clip to the area of interest
  # Pre flight checks...
  optram_func <- as.character(match.call()[[1]])
  if (!check_scihub_access(scihub_user, scihub_pass, optram_func)) {
    return(NULL)
  }
  if (!file.exists(aoi_file)) {
      warning("An area_of_interest polygon shapefile is required",
      "\n", "Please prepare the area_of_interest boundary file.")
  } else {
      tryCatch({terra::vect(aoi_file)},
                error = function(e) {
                  warning(aoi_file, ": is not a recognized spatial format")
                  return(NULL) }
      )
  }

  # Avoid "no visible binding for global variable" NOTE
  aoi_name  <- result_list <- NULL
  # Checks OK, proceed to download
  aoi_name <- rOPTRAM::aoi_to_name(aoi_file)
  # Get generic config json 
  config_file <- system.file("extdata", "s2_config.json", package = "rOPTRAM")
  result_list <- sen2r::sen2r(
      param_list = config_file,
      gui = FALSE,
      server = c("scihub", "gcloud"),
      rm_safe = remove_safe,
      max_cloud_safe = max_cloud * 1.5,
      max_mask = max_cloud,
      timewindow = c(from_date, to_date),
      timeperiod = timeperiod,
      list_prods = "BOA",
      # param name in `sen2r` is "list_indices"
      # in rOPTRAM: "list_indicies"
      list_indices = list_indicies,
      resampling = "bilinear",
      extent = aoi_file,
      extent_name = aoi_name,
      extent_as_mask = TRUE,
      path_l2a = output_dir,
      path_out = output_dir,
      path_indices = output_dir,
      thumbnails = FALSE
    )
  return(result_list)
}
