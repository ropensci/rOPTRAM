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
#' aoi <- system.file("extdata", "migda.gpkg", package = 'rOPTRAM')
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

#' @title Utility Function to Acquire Sentinel-2 Imagery using CDSE and the 
#' dataspace of Copernicus
#' @description This non-exported function uses the `CDSE` package
#' to send a request to Copernicus dataspace, and prepare the products.
#' Called by optram_acquire_s2
#' @param aoi_file, string, full path to polygon spatial file of
#'      boundary of area of interest
#' @param from_date, string, represents start of date range,
#'      formatted as "YYYY-MM-DD"
#' @param to_date, string, end of date range, formatted as "YYYY-MM-DD"
#' @param max_cloud, integer, maximum percent of cloud cover. Default 10.
#' @param timeperiod, string, either "full" for the whole date range,
#' or "seasonal" for only months specified, but over the full date range 
#' - currently not in use
#' @param output_dir, string, path to save downloaded, and processed imagery
#' @param veg_index, string, which index to prepare. Default "NDVI".
#'  Can be "NDVI", "SAVI", "MSAVI", etc
#' @param remove_safe, string, "yes" or "no":
#'      whether to delete downloaded SAFE directories
#'      after processing, default "yes" - currently not in use
#' @param save_creds, logical, whether to save CDSE credentials. Default TRUE.
#' @param clientid, string, user's OAuth client id. Required if `save_creds` 
#'      is TRUE.
#' @param secret, string, user's OAuth secret. Required if `save_creds` is TRUE.
#' @return void - extracting the images inside the function
#' @note
#' This function utilizes the `CDSE` package.
#' Make sure to install the CDSE and jsonlite packages.
#' Create OAuth account and token:
#' Creating an Account:
#'  1. Navigate to the [Copernicus portal](https://dataspace.copernicus.eu/).
#'  2. Click the "Register" button to access the account creation page.
#'  3. If already registered, enter your username and password, 
#'    and click "Login."
#'  4. Once logged in, go to the User dashboard and click "User Settings" to 
#'    access the Settings page.
#' 
#' Creating OAuth Client:
#'  1. On the Settings page, click the green "Create New" button located on 
#'    the right.
#'  2. Enter a suitable "Client Name" and click the green "Create Client" 
#'    button.
#'  3. A Client secret is generated.
#' the user must save her secret and clientid somewhere.
#' these credentials will be saved automatically to a standard filesystem 
#' location if the user calls check_scihub() with the argument save_creds 
#' set to TRUE (recommended).
#' if the user chooses not to save credentials to the standard filesystem 
#' location, then she will need to add both clientid and secret to each 
#' acquire_scihub() function call.
#' 
#' Using Credentials with `aquire_scihub`:
#'  - Now, you can utilize the generated `clientid` and `secret` in 
#'    the `aquire_scihub` function.
#'  - If you want to store your credentials on your computer, ensure that when 
#'    running `aquire_scihub`, the `save_creds` parameter is set to `TRUE`.
#'  - During the first run of `aquire_scihub`, manually input your `clientid` 
#'    and `secret` in the function's signature. Subsequent runs will use the 
#'    stored credentials.
#' @examples
#' \dontrun{
#' from_date <- "2018-12-01"
#' to_date <- "2019-04-30"
#' aoi <- system.file("extdata", "migda_aoi.gpkg", package = 'rOPTRAM')
#' acquire_scihub(aoi, from_date, to_date,
#'                timeperiod = "full",
#'                veg_index = "SAVI")
#' }
acquire_scihub <- function(
    aoi_file,
    from_date, to_date,
    max_cloud = 10,
    timeperiod = "full",
    output_dir = tempdir(),
    remove_safe = "yes",
    safe_dir = output_dir,
    veg_index = "NDVI",
    save_creds = TRUE,
    clientid = NULL,
    secret = NULL) {
  
  aoi <- sf::read_sf(aoi_file, as_tibble = FALSE)
  
  # Retrieve OAuth token using credentials from file directory
  tok <- check_scihub(clientid = clientid, secret = secret, 
                      save_creds = save_creds)
  if (is.null(tok)){
    message("No CDSE token found. Exiting...")
    return(NULL)
  }
  # Create a folder for the BOA in the output directory
  result_folder_boa <- file.path(output_dir, "BOA")
  
  # Check if the folder already exists; if not, create it
  if (!dir.exists(result_folder_boa)) {
    dir.create(result_folder_boa)
  }
  
  # Create a folder named after veg_index in the output directory
  result_folder_vi <- file.path(output_dir, veg_index)
  
  # Check if the folder already exists; if not, create it
  if (!dir.exists(result_folder_vi)) {
    dir.create(result_folder_vi)
  }
  
  # Create a folder for the STR in the output directory
  result_folder_str <- file.path(output_dir, "STR")
  
  # Check if the folder already exists; if not, create it
  if (!dir.exists(result_folder_str)) {
    dir.create(result_folder_str)
  }
  
  # Retrieve the necessary scripts
  script_file_boa <- system.file("scripts", "BOA.js", package = "rOPTRAM")
  script_file_str <- system.file("scripts", "STR.js", package = "rOPTRAM")
  script_file_vi <- system.file("scripts", paste0(veg_index, ".js"), 
                                package = "rOPTRAM")
  
  img_list <- CDSE::SearchCatalog(aoi = aoi,
                                  from = from_date, to = to_date,
                                  collection = "sentinel-2-l2a",
                                  token = tok)
  # filter out cloud cover
  img_list <- img_list[img_list$tileCloudCover < max_cloud,]
  
  # Retrieve the images in BOA,STR and VI formats
  get_result_list <- function(script_vi, s_dir){
    result_list <- lapply(img_list$acquisitionDate, function(d){
      time_range <- as.character(d)
      result_rast <- CDSE::GetArchiveImage(aoi = aoi,
                                     time_range = time_range,
                                     script = script_vi,
                                     collection = "sentinel-2-l2a",
                                     format = "image/tiff",
                                     mask = TRUE,
                                     resolution = c(10,10),
                                     token = tok)
      
      raster_file <- file.path(s_dir, paste0("CDSE_",
                                             as.character(time_range),
                                             ".tif"))
      writeRaster(result_rast, raster_file, overwrite = TRUE)
      return(raster_file)
    })
    return(result_list)
  }
  
  result_boa <- unlist(get_result_list(script_file_boa, result_folder_boa))
  result_str <- unlist(get_result_list(script_file_str, result_folder_str))
  result_vi <- unlist(get_result_list(script_file_vi, result_folder_vi))
  
  saveRDS(result_boa, file.path(output_dir, "result_list_boa.rds"))
  saveRDS(result_str, file.path(output_dir, "result_list_str.rds"))
  saveRDS(result_vi, file.path(output_dir, "result_list_vi.rds"))
  
}


#' @title Check access to scihub API
#' @description  The check_scihub function verifies the availability of a 
#' CDSE API, retrieves credentials from a file or by provided clientid and 
#' secret, obtains an OAuthClient through CDSE::GetOAuthClient(), and 
#' optionally saves the credentials to a file before returning the OAuthClient.
#' @param clientid The client identifier used for authentication with CDSE.
#' @param secret The secret key used for authentication with CDSE.
#' @param save_creds A logical parameter indicating whether to save the 
#' provided credentials to a file for future use.
#' @return OAuthClient
#' @noRd
#' @examples
#' \dontrun{
#' scihub_ok <- check_scihub(clientid = NULL, secret = NULL, save_creds = FALSE)
#' }
#'
check_scihub <- function(clientid = NULL, secret = NULL, save_creds = FALSE) {
  
  CDSE_ok <- "CDSE" %in% utils::installed.packages()
  if (!CDSE_ok) {
    message("CDSE package is missing. Download is not possible",
            "\n", "Exiting...")
    return(NULL)
  }
  
  jsonlite_ok <- "jsonlite" %in% utils::installed.packages()
  if (!jsonlite_ok) {
    message("jsonlite package is missing. Download is not possible",
            "\n", "Exiting...")
    return(NULL)
  }
  
  # If clientid or secret is null, look for cdse_credentials file
  if (is.null(clientid) || is.null(secret)) {
    creds <- retrieve_cdse_credentials()
    if (is.null(creds)) {
      message("No CDSE credentials found. Exiting...")
      return(NULL)
    }
    else {
      clientid_value <- as.character(creds[[1]]$clientid)
      secret_value <- as.character(creds[[1]]$secret)
      oAuthClient <- CDSE::GetOAuthToken(id = clientid_value
                                         ,secret = secret_value)
    }
  } else {
    # If clientid and secret are NOT null, run CDSE::GetOAuthToken()
    tryCatch({
      oAuthClient <- CDSE::GetOAuthToken(id = clientid, secret = secret)
      if (save_creds) {
        # If successful and save_creds is TRUE, write clientid and secret 
        # to cdse_credentials file
        store_cdse_credentials(clientid = clientid, secret = secret)
      }
      return(oAuthClient)
    }, error = function(e) {
      message("Error in retrieving CDSE credentials: ", 
              conditionMessage(e), "\n")
      return(NULL)
    })
  }
}

#' @title Utility Function to Acquire Sentinel-2 Imagery using openeo and the 
#' dataspace of Copernicus
#' @description This non-exported function uses the `openeo` package
#' to send a request to Copernicus dataspace, and prepare the products.
#' Called by optram_acquire_s2
#' @param aoi_file, string, full path to polygon spatial file of
#'      boundary of area of interest
#' @param from_date, string, represents start of date range,
#'      formatted as "YYYY-MM-DD"
#' @param to_date, string, end of date range, formatted as "YYYY-MM-DD"
#' @param max_cloud, integer, maximum percent of cloud cover. Default 10.
#' @param timeperiod, string, either "full" for the whole date range,
#' or "seasonal" for only months specified, but over the full date range 
#' - currently not in use
#' @param output_dir, string, path to save downloaded, and processed imagery
#' @param veg_index, string, which index to prepare. Default "NDVI".
#'  Can be "NDVI", "SAVI", "MSAVI", etc
#' @param remove_safe, string, "yes" or "no":
#'      whether to delete downloaded SAFE directories
#'      after processing, default "yes" - - currently not in use
#' @param scale_factor, integer, scaling factor for EO data source default 10000
#' , to scale Sentinel-2 15 bit DN to range (0, 1)

#' @return void - extracting the images inside the function
#' @note
#' This function utilizes the `openeo` package.
#' Instructions for the login process:
#' First of all, to authenticate your account on the backend of the Copernicus
#' Data Space Ecosystem, it is necessary for you to complete the registration 
#' process. Follow these instructions for registration:
#' https://documentation.dataspace.copernicus.eu/Registration.html
#' After you have registered and installed the `openeo` package, you can run the
#' `acquire_openeo` function.
#' During the process of connecting to the server and logging in, you need to 
#' follow these steps:
#' A. When the message "Press <enter> to proceed:" appears in the console, 
#' press enter. 
#' Calling this method opens your system web browser, with which 
#' you can authenticate yourself on the back-end authentication system. After 
#' that, the website will give you instructions to go back to the R client, 
#' where your connection has logged your account in. This means that every 
#' call that comes after that via the connection variable is executed by your 
#' user account.
#' B. You will be redirected to "https://identity.dataspace.copernicus.eu/". 
#' Ensure you have an account and are logged in. You will be required to 
#' grant access - press "yes".
#'
#' @examples
#' \dontrun{
#' from_date <- "2018-12-01"
#' to_date <- "2019-04-30"
#' aoi <- system.file("extdata", "migda_aoi.gpkg", package = 'rOPTRAM')
#' acquire_openeo(aoi, from_date, to_date,
#'                timeperiod = "full",
#'                veg_index = "SAVI")
#' }
acquire_openeo <- function(
    aoi_file,
    from_date, to_date,
    max_cloud = 10,
    timeperiod = "full",
    output_dir = tempdir(),
    remove_safe = "yes",
    safe_dir = output_dir,
    veg_index = "NDVI",
    scale_factor = 10000) { 
  
  if(!check_openeo()) return(NULL)
  
  # Extracting bbox from the aoi file
  catchment <- sf::st_read(aoi_file)
  bbox <- sf::st_bbox(obj = catchment)
  
  # get the process collection to use the predefined processes of the back-end
  p <- openeo::processes()
  
  # get the collection list to get easier access to the collection ids, 
  # via auto completion
  collections <- openeo::list_collections()
  
  # get the formats
  formats <- openeo::list_file_formats()
  
  # load the initial data collection and limit the amount of data loaded
  # note: for the collection id and later the format you can also use the
  # its character value
  cube_s2 <- p$load_collection(id = collections$SENTINEL2_L2A,
                              spatial_extent = bbox,
                              temporal_extent = c(from_date, to_date),
                              bands = c('B01', 'B02', 'B03', 'B04', 'B05', 
                                        'B06', 'B07', 'B08', 'B8A', 'B09', 
                                        'B11', 'B12'),
                              properties = list(
                                "eo:cloud_cover" = function(x) x <= max_cloud))
  
  # Create a folder for the BOA in the output directory
  result_folder_boa <- file.path(output_dir, "BOA")
  
  # Check if the folder already exists; if not, create it
  if (!dir.exists(result_folder_boa)) {
    dir.create(result_folder_boa)
  }
  
  # Create a folder named after veg_index in the output directory
  result_folder_vi <- file.path(output_dir, veg_index)
  
  # Check if the folder already exists; if not, create it
  if (!dir.exists(result_folder_vi)) {
    dir.create(result_folder_vi)
  }
  
  # Create a folder for the STR in the output directory
  result_folder_str <- file.path(output_dir, "STR")
  
  # Check if the folder already exists; if not, create it
  if (!dir.exists(result_folder_str)) {
    dir.create(result_folder_str)
  }
  
  # Calculate Vegetation Index function
  calculate_vi_ <- function(x, context){
    
    # loading bands colors
    blue <- x["B02"]
    green <- x["B03"]
    red <- x["B04"]
    nir <- x["B08"]
    
    if (veg_index == "NDVI") {
      vi_rast <- ((nir - red) / (nir + red))
    } else if (veg_index == "SAVI") {
      vi_rast <- ((1.5 * (nir - red)) / (nir + red + 0.5) )
    } else if (veg_index == "MSAVI") {
      vi_rast <- ((2 * nir + 1 - sqrt((2 * nir + 1)^2 - 
                                       8 * (nir - red))) / 2)
    } else if (veg_index == "CI") {
      vi_rast <- (1-((red - blue) / (red + blue)))
    } else if (veg_index == "BSCI") {
      vi_rast <- ((1-(2*(red - green))) / 
                   (terra::mean(green, red, nir, na.rm = TRUE)))
    } else {
      message("Unrecognized index: ", veg_index)
      vi_rast <- NULL
    }
    return(vi_rast)
  }
  
  # Calculate STR from SWIR Bottom of Atmosphere Band
  calculate_str_ <- function(x, context){ 
    SWIR_DN <- x['B11']
    SWIR <-  SWIR_DN / scale_factor
    # Convert from Solar irradiance
    # solar_irradiance_12 <- 87.25
    # SWIR <- (SWIR_irr/10) * solar_irradiance_12
    STR <- (1 - SWIR)^2 / (2*SWIR)
    return(STR)
  }
  
  
  cube_s2_vi <- p$reduce_dimension(data = cube_s2, reducer = calculate_vi_, 
                                  dimension = "bands")
  cube_s2_str <- p$reduce_dimension(data = cube_s2, reducer = calculate_str_, 
                                   dimension = "bands")
  cube_S2_boa <- p$resample_spatial(data = cube_s2, resolution = 10, 
                                   method = "near")
  
  result_vi <- p$save_result(data = cube_s2_vi, format = formats$output$GTiff)
  result_str <- p$save_result(data = cube_s2_str, format = formats$output$GTiff)
  result_boa <- p$save_result(data = cube_S2_boa, format = formats$output$GTiff)
  
  job_vi <- openeo::create_job(graph = result_vi, title = "vi files")
  job_str <- openeo::create_job(graph = result_str, title = "str files")
  job_boa <- openeo::create_job(graph = result_boa, title = "BOA files") 
  
  # then start the processing of the job and turn on logging,
  # (messages that are captured on the back-end during the process execution)
  openeo::start_job(job = job_vi, log = TRUE)
  openeo::start_job(job = job_str, log = TRUE)
  openeo::start_job(job = job_boa, log = TRUE)
  
  check_job_status <- function(job) {
    while (openeo::describe_job(job)$status != "finished") {
      Sys.sleep(8)  # Sleep for 8 seconds before checking again
      message("job still running")
      if (openeo::describe_job(job)$status == "error") {
        message("Error: Job status is 'error'. Additional details:")
        message(openeo::describe_job(job))
        return(NULL)
      }
    }
    return("finished")
  }
  
  job_vi_status <- check_job_status(job_vi)
  job_str_status <- check_job_status(job_str)
  job_boa_status <- check_job_status(job_boa)
  
  if(job_boa_status != "finished" | job_vi_status != "finished" 
     | job_str_status != "finished") return(NULL)
  
  message("finished succesfully")
  Sys.sleep(5)
  
  # list the processed results
  jobs_vi <- openeo::list_results(job = job_vi)
  jobs_str <- openeo::list_results(job = job_str)
  jobs_boa <- openeo::list_results(job = job_boa)
  
  # download all the files into a folder on the file system
  openeo::download_results(job = job_vi, folder = result_folder_vi)
  openeo::download_results(job = job_str, folder = result_folder_str)
  openeo::download_results(job = job_boa, folder = result_folder_boa)
}

#' @title Check Access to Copernicus openEO
#' @description Check access and authentication to the openEO platform, 
#' and ensure that the openeo library is installed.

#' @return boolean
#' @noRd
#' @examples
#' \dontrun{
#' openeo_ok <- check_openeo()
#' }
#'@note
#' Instructions for the login process:
#' A. When the message "Press <enter> to proceed:" appears in the console, 
#' press enter.
#' B. You will be redirected to "https://identity.dataspace.copernicus.eu/". 
#' Ensure you have an account and are logged in. You will be required to 
#' grant access - press "yes".

check_openeo <- function() {
  
  openeo_ok <- "openeo" %in% utils::installed.packages()
  if (!openeo_ok) {
    message("openeo package is missing. Download is not possible",
            "\n", "Exiting...")
    return(FALSE)
  }
  
  # Connect to the back-end
  tryCatch({
    conn <- openeo::connect(host = "https://openeo.dataspace.copernicus.eu")
    
    # Check if the connection is successful
    if (!conn$isConnected()) {
      message("Connection to the back-end failed.")
    }
  }, error = function(e) {
    message("Error connecting to the back-end: ", conditionMessage(e), "\n")
    return(FALSE)
  })
  
  # Login
  tryCatch({
    # Check if the connection object exists
    if (exists("conn") && !is.null(conn)) {
      # Attempt to log in
      openeo::login()
      
      # Check if the login is successful
      if (!conn$isLoggedIn()) {
        message("Login failed.")
      }
    } else {
      message("Connection object is missing.")
    }
  }, error = function(e) {
    message("Error during login: ", conditionMessage(e), "\n")
    return(FALSE)
  })
  
  return(TRUE)
}