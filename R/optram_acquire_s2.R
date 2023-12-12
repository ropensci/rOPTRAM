#' @title Acquire Sentinel 2 Images at a Given Location and Date Range
#' @description Use the `sen2r` package to acquire, preprocess and crop
#'  Sentinel 2 satellite imagery.
#'  L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020)
#'  sen2r: An R toolbox for automatically downloading and preprocessing
#'  Sentinel-2 satellite data
#'  Computers & Geosciences, 139, 104473
#'  doi: 10.1016/j.cageo.2020.104473
#'  URL: https://sen2r.ranghetti.info/.
#'  The package prepares vegetation indices, cropped to an area of interest.
#' @param aoi_file, string, full path to polygon spatial file of
#'      boundary of area of interest
#' @param from_date, string, represents start of date range,
#'      formatted as "YYYY-MM-DD"
#' @param to_date, string, end of date range, formatted as "YYYY-MM-DD"
#' @param max_cloud, integer, maximum percent of cloud cover. Default 15.
#' @param timeperiod, string, either "full" for the whole date range,
#' or "seasonal" for only months specified, but over the full date range.
#' @param output_dir, string, path to save downloaded, and processed imagery
#' @param veg_index, string, which index to prepare. Default "NDVI".
#'  Can be "NDVI", "SAVI", "MSAVI", etc
#' @param remove_safe, string, "yes" or "no":
#'      whether to delete downloaded SAFE directories
#'      after processing, default "yes"
#' @param remote, string, from which archive to download imagery
#'    possible values: 'gcloud', 'scihub', 'openeo'
#' @return output_path, string, path to downloaded files
#' @export
#' @note
#' This wrapper function calls one of multiple download functions,
#' each accessing a different cloud-based resource.
#' The cloud based resource can be one of:
#' "gcloud",...
#' If "gcloud" then:
#' The  \CRANpkg{sen2r} function from the {sen2r} package is used.
#' This function acquires Sentinel 2 imagery, clips to aoi,
#' and prepares multiband output rasters, save to the `output_dir`.
#' Only L2A (atmospherically corrected) images are acquired, and therefore:
#'  - only one NIR band, B08 is used. The lower res B8A is not used
#'  - furthermore, the cirrus band B09 is not relevant for BOA level
#'  - so band 10 is the SWIR reflectance at 1600 nm,
#'    and band 11 is reflectance at 2200 nm.
#' The \CRANpkg{sen2r} package uses `gsutil`, a utility in the Google Cloud SDK
#' to download imagery. Please first install `gcloud` following instructions:
#' https://cloud.google.com/sdk/docs/install
#' for your operating system.
#' And be sure to initialize with you google username and password.
#' If the Google Cloud CLI is not installed, this function fails
#'
#' @examples
#' \dontrun{
#' from_date <- "2018-12-01"
#' to_date <- "2019-04-30"
#' aoi <- system.file("extdata", "migda_aoi.gpkg", package = 'rOPTRAM')
#' s2_file_list <- optram_acquire_s2(aoi,
#'                                  from_date, to_date,
#'                                  timeperiod = "full",
#'                                  veg_index = "SAVI")
#' }

optram_acquire_s2 <- function(
      aoi_file,
      from_date, to_date,
      max_cloud = 10,
      timeperiod = "full",
      output_dir = tempdir(),
      remove_safe = "yes",
      veg_index = "NDVI",
      remote = "gcloud") {
  # Avoid "no visible binding for global variable" NOTE
  gcloud <- scihub <- openeo <- NULL

  # Pre flight checks...
  if (!check_aoi(aoi_file)) return(NULL)

  remote <- match.arg(remote)

  switch(remote,
         gcloud = acquire_gcloud(aoi_file,
                                 from_date, to_date,
                                 max_cloud = 10,
                                 timeperiod = "full",
                                 output_dir = tempdir(),
                                 remove_safe = "yes",
                                 veg_index = "NDVI"),
         scihub = acquire_scihub(aoi_file,
                                from_date, to_date,
                                max_cloud = 10,
                                timeperiod = "full",
                                output_dir = tempdir(),
                                remove_safe = "yes",
                                veg_index = "NDVI"),
         openeo = acquire_openeo(aoi_file,
                                 from_date, to_date,
                                 max_cloud = 10,
                                 timeperiod = "full",
                                 output_dir = tempdir(),
                                 remove_safe = "yes",
                                 veg_index = "NDVI")
         )
}
