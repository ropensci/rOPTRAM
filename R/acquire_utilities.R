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

    if(!check_gcloud()) return(NULL)
    # Make sure output_dir exists
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    # Checks OK, proceed to download

    aoi_name <- aoi_to_name(aoi_file)
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


#' @title Check if gcloud is installed
#' @description  Check for sen2r dependency, and gcloud utility
#' @return boolean
#' @noRd
#' @examples
#' \dontrun{
#' gcloud_ok <- check_gcloud()
#' }
#'
check_gcloud <- function() {
  # Pre flight checks
  sen2r_ok <- "sen2r" %in% utils::installed.packages()
  if (!sen2r_ok) {
    message("sen2r package is missing. Download is not possible",
            "\n", "Exiting...")
    return(FALSE)
  }
  if (Sys.info()['sysname'] == 'Windows') {
    # Assume that gcloud-sdk is installed in USER's home dir
    # Get the first instance of gsutil file
    homedir <- Sys.getenv("USERPROFILE")
    gsutil_path <- system2("WHERE",
                           paste("/R", homedir, "gsutil"), stdout = TRUE)[1]
  } else {
    gsutil_path <- Sys.which("gsutil")
  }
  gcloud_ok <- ifelse(is.null(gsutil_path) | gsutil_path == "" |
                      is.na(gsutil_path) | !sen2r::is_gcloud_configured(),
                      FALSE,
                      sen2r::check_gcloud(gsutil_path, check_creds = FALSE))

  if (gcloud_ok) {
    message("Using gcloud CLI")
    return(TRUE)
  } else {
    message("No access to Google cloud", "\n", "Exiting")
    return(FALSE)}
}


acquire_scihub <- function(aoi_file,
                           from_date, to_date,
                           max_cloud = 10,
                           timeperiod = "full",
                           output_dir = tempdir(),
                           remove_safe = "yes",
                           veg_index = "NDVI") {
  # TODO: implement this function using new scihub API
  # Check for token, etc...
  # Write eval_functions (in javascript) for each vegetation index
  if(!check_scihub()) return(NULL)
}


#' @title Check access to scihub API
#' @description  Check access, and Oauth to scihub API
#' @return boolean
#' @noRd
#' @examples
#' \dontrun{
#' scihub_ok <- check_scihub()
#' }
#'
check_scihub <- function() {
  # TODO: this is a stub, just for test_that
  return(FALSE)
}

acquire_openeo <- function(aoi_file,
                           from_date, to_date,
                           max_cloud = 10,
                           timeperiod = "full",
                           output_dir = tempdir(),
                           remove_safe = "yes",
                           veg_index = "NDVI") {
  # TODO: implement this function using new openeo
  # Check for token, etc...
  # Write eval_functions (in javascript) for each vegetation index
  if(!check_openeo()) return(NULL)
  
  # Extracting bbox from the aoi file
  catchment = sf::st_read(aoi_file)
  bbox = sf::st_bbox(obj = catchment)
  
  # get the process collection to use the predefined processes of the back-end
  p = processes()
  
  # get the collection list to get easier access to the collection ids, via auto completion
  collections = list_collections()
  
  # get the formats
  formats = list_file_formats()
  
  # load the initial data collection and limit the amount of data loaded
  # note: for the collection id and later the format you can also use the its character value
  cube_s2 = p$load_collection(id = collections$SENTINEL2_L2A,
                              spatial_extent = bbox,
                              temporal_extent = c(from_date, to_date),
                              bands = c("B02", "B03", "B04", "B08"),
                              properties = list(
                                "eo:cloud_cover" = function(x) x <= max_cloud))
  
  # Create a folder for the BOA in the output directory
  result_folder_BOA <- file.path(output_dir, "BOA")
  
  # Check if the folder already exists; if not, create it
  if (!dir.exists(result_folder_BOA)) {
    dir.create(result_folder_BOA)
  }
  
  # Create a folder named after veg_index in the output directory
  result_folder_vi <- file.path(output_dir, veg_index)
  
  # Check if the folder already exists; if not, create it
  if (!dir.exists(result_folder_vi)) {
    dir.create(result_folder_vi)
  }
  
  # Calculate Vegetation Index function
  calculate_vi_ <- function(x, context){
    
    # loading bands colors
    blue = x[1]
    green = x[2]
    red = x[3]
    nir = x[4]
    
    if (veg_index == "NDVI") {
      vi_rast = ((nir - red) / (nir + red))
    } else if (veg_index == "SAVI") {
      vi_rast = ((1.5 * (nir - red)) / (nir + red + 0.5) )
    } else if (veg_index == "MSAVI") {
      vi_rast = ((2 * nir + 1 - sqrt((2 * nir + 1)^2 - 
                                       8 * (nir - red))) / 2)
    } else if (veg_index == "CI") {
      vi_rast = (1-((red - blue) / (red + blue)))
    } else if (veg_index == "BSCI") {
      vi_rast = ((1-(2*(red - green))) / 
                   (terra::mean(green, red, nir, na.rm = TRUE)))
    } else {
      message("Unrecognized index: ", veg_index)
      vi_rast = NULL
      return(NULL)
    }
    return(vi_rast)
  }
  
  cube_s2_vi = p$reduce_dimension(data = cube_s2, reducer = calculate_vi_, dimension = "bands")
  cube_S2_boa = p$resample_spatial(data = cube_s2, resolution = 10, method = "near")
  
  result_vi = p$save_result(data = cube_s2_vi, format = formats$output$GTiff)
  result_boa = p$save_result(data = cube_S2_boa, format = formats$output$GTiff)
  
  job_vi = create_job(graph = result_vi, title = "vi files")
  job_BOA = create_job(graph = result_boa, title = "BOA files") 
  
  # then start the processing of the job and turn on logging (messages that are captured on the back-end during the process   execution)
  start_job(job = job_vi, log = TRUE)
  start_job(job = job_BOA, log = TRUE)
  
  check_job_status <- function(job) {
    while (describe_job(job)$status != "finished") {
      Sys.sleep(2)  # Sleep for 2 seconds before checking again
      print("job still running")
      if (describe_job(job)$status == "error") {
        print("Error: Job status is 'error'. Additional details:")
        print(describe_job(job))
        return(NULL)
      }
    }
    return("finished")
  }
  
  job_BOA_status = check_job_status(job_BOA)
  job_vi_status = check_job_status(job_vi)
  
  if(job_BOA_status != "finished" | job_vi_status != "finished") return(NULL)
  
  print("finished succesfully")
  Sys.sleep(5)
  
  # list the processed results
  jobs_boa = list_results(job = job_BOA)
  jobs_vi = list_results(job = job_vi)
  
  # download all the files into a folder on the file system
  download_results(job = job_BOA, folder = result_folder_BOA)
  download_results(job = job_vi, folder = result_folder_vi)
}

#' @title Check access to Copernicus openEO
#' @description  Check access and Oauth to openEO platform
#' @return boolean
#' @noRd
#' @examples
#' \dontrun{
#' openeo_ok <- check_openeo()
#' }
#'
check_openeo <- function() {
  openeo_ok <- "openeo" %in% utils::installed.packages()
  if (!openeo_ok) {
    message("openeo package is missing. Download is not possible",
            "\n", "Exiting...")
    return(FALSE)
  }
  
  sf_ok <- "sf" %in% utils::installed.packages()
  if (!sf_ok) {
    message("sf package is missing. Download is not possible",
            "\n", "Exiting...")
    return(FALSE)
  }
  
  terra_ok <- "terra" %in% utils::installed.packages()
  if (!terra_ok) {
    message("terra package is missing. Download is not possible",
            "\n", "Exiting...")
    return(FALSE)
  }
  
  # Connect to the back-end
  tryCatch({
    conn <- connect(host = "https://openeo.dataspace.copernicus.eu")
    
    # Check if the connection is successful
    if (!conn$isConnected()) {
      stop("Connection to the back-end failed.")
    }
  }, error = function(e) {
    cat("Error connecting to the back-end: ", conditionMessage(e), "\n")
    return(FALSE)
  })
  
  # Login
  tryCatch({
    # Check if the connection object exists
    if (exists("conn") && !is.null(conn)) {
      # Attempt to log in
      login()
      
      # Check if the login is successful
      if (!conn$isLoggedIn()) {
        stop("Login failed.")
      }
    } else {
      stop("Connection object is missing.")
    }
  }, error = function(e) {
    cat("Error during login: ", conditionMessage(e), "\n")
    return(FALSE)
  })
  
  return(TRUE)
}
