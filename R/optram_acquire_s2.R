#' Acquire Sentinel 2 images at a given location and date range
#' 
#' @description Use the `sen2r` package to acquire, preprocess and crop, Sentinel 2 satellite imagery.
#' L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020)
#' sen2r: An R toolbox for automatically downloading and preprocessing Sentinel-2 satellite data
#' Computers & Geosciences, 139, 104473
#' doi: 10.1016/j.cageo.2020.104473
#' URL: https://sen2r.ranghetti.info/.
#' The package prepares vegetation indicies, cropped to an area of interest.
#' 
#' @param aoi: string, full path to polygon spatial file of boundary of area of interest
#' @param from_date: string, represents start of date range, formatted as "YYYY-MM-DD"
#' @param to_date: string, end of date range, formatted as "YYYY-MM-DD"
#' @param max_cloud: integer, maximum cloud cover percent. Default 15. 
#' @param scihub_user: string, username on Copernicus scihub
#' @param scihub_pass: string, password for access to Copernicus scihub
#' @param timeperiod:string, either "full" for the whole date range,
#' or "seasonal" for only months specified, but over the full date range.
#' @param output_dir: string, path to save downloaded, and processed imagery
#' @param list_indices: vector of strings, which indicies to prepare. Default c("NDVI", "SAVI")
#' 
#' @return output_path: string, path to downloaded files
#' @export
#' @examples
#' from_date <- "2018-12-01"
#' to_date <- "2019-04-30" 
#' aoi <- "extdata/migda_9.gpkg"
#' s2_file_list <- optram_acquire_s2(aoi,
#'                                  from_date, to_date,
#'                                  scipub = file.path("~", "apihub.txt"),
#'                                  list_indices="MSAVI2")

optram_acquire_s2 <- function(aoi,
                            from_date, to_date,
                            scipub_user, scihub_pass,
                            max_cloud = 15,
                            timeperiod = "full",
                            output_dir = tempdir(),
                            list_indices = c("NDVI","SAVI")
                            ) {
    # Download Sentinel 2 images during the requested date range,
    # and clip to the area of interest
    # Pre flight checks...
    if (!require(sen2r)) {
        warning("This function requires the `sen2r` package.", "\n",
        "Please install that package before running `acquire_s2_images.R`")
        return(NULL)
    }
    if (!sen2r::is_scihub_configured()) {
        if(sen2r::check_scihub_login(scihub_user, scihub_pass)) {
            sen2r_path <- file.path("~", ".sen2r")
            if (!dir.exists(sen2r_path)) {
                dir.create(sen2r_path)
            }
            apihub_path <- file.path(sen2r_path, "apihub.txt")
            sen2r::write_scihub_login(scihub_user, scihub_pass, apihub_path = apihub_path)
        } else {
            warning("Login credentials not accepted by ESA Sentinel Hub",
            "\n", "Please register on Sentinel Hub and retry.")
            return(NULL)
        }
    }
    if (!file.exists(aoi)) {
        warning("An area_of_interest polygon shapefile is required",
        "\n", "Please prepare the area_of_interest boundary file.")
    } else {
        tryCatch({terra::vect(aoi)},
                  error = function(e) {
                    warning(aoi, ":is not a recognized spatial format")
                    return(NULL) }
        )
    }
    # Checks OK, proceed to download
    aoi_name <- tools::file_path_sans_ext(basename(aoi))
    aoi_name <- gsub(x = aoi_name, pattern = " ", replacement = "_")
    aoi_name <- gsub(x = aoi_name, pattern = "\\.", replacement = "_")
    aoi_name <- tools::toTitleCase(aoi_name)
    result_list <- sen2r::sen2r(
        gui = FALSE,
        server = c("scihub", "gcloud"),
        rm_safe = "yes",
        max_cloud_safe = max_cloud,
        overwrite = TRUE,
        timewindow = c(from_date, to_date),
        timeperiod = timeperiod,
        list_prods = "BOA",
        list_indices = list_indicies,
        resampling = "bilinear",
        extent = aoi,
        extent_name = aoi_name,
        extent_as_mask = TRUE
      )
    return(result_list)
}
