#' @title Utility Function to Acquire Sentinel-2 Imagery using Google Cloud CLI
#' @description This non-exported function uses the `sen2r` package 
#' to send a request to Google Cloud, and prepare the products.
#' Called by optram_acquire_s2
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
#' @return output_path, string, path to downloaded files
#' @note
#' This function calls the `sen2r` function from the \CRANpkg{sen2r} package.
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

acquire_gcloud <- function(aoi_file,
                        from_date, to_date,
                        max_cloud = 10,
                        timeperiod = "full",
                        output_dir = tempdir(),
                        remove_safe = "yes",
                        veg_index = "NDVI") {
    # Avoid "no visible binding for global variable" NOTE
    result_list <- aoi_name <- NULL
    # Pre flight checks done in advance by optram_acquire_s2()
    aoi_name <- rOPTRAM:::aoi_to_name(aoi_file)
    result_list <- sen2r::sen2r(
        gui = FALSE,
        server = "gcloud",
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
