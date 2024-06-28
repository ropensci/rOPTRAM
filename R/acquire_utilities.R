#' @title Utility Function to Acquire Sentinel-2 Imagery Using \code{CDSE} Package
#' @description This function uses the \code{CDSE} package
#' to send a request to Copernicus Dataspace, and prepare the products.
#' Called by \code{\link[rOPTRAM]{optram_acquire_s2}}
#' @param aoi, {sf} object, POLYGON or MULTIPOLYGON of area of interest
#' @param from_date, string, represents start of date range,
#'      formatted as "YYYY-MM-DD"
#' @param to_date, string, end of date range, formatted as "YYYY-MM-DD"
#' @param output_dir, string, path to save downloaded, and processed imagery
#' @param save_creds, logical, whether to save CDSE credentials. Default TRUE.
#' @param clientid, string, user's OAuth client id. Required if \code{save_creds}
#'      is TRUE.
#' @param secret, string, user's OAuth secret. Required if \code{save_creds} is TRUE.
#' @export
#' @return list of BOA files
#' @note
#' #' This function utilizes the \code{CDSE} package.
#' Make sure to install the \code{CDSE} and \code{jsonlite} packages.
#' Create OAuth account and token:
#' Creating an Account:
#' \enumerate{
#'  \item Navigate to the \url{https://dataspace.copernicus.eu/}.
#'  \item Click the "Register" button to access the account creation page.
#'  \item If already registered, enter your username and password, and click "Login."
#'  \item Once logged in, go to the User dashboard and click "User Settings" to access the Settings page.
#' }
#' Creating OAuth Client:
#' \enumerate{
#'   \item On the Settings page, click the green "Create New" button located on the right.
#'   \item Enter a suitable "Client Name" and click the green "Create Client" button.
#'   \item A Client secret is generated.
#'   }
#' The user must save her secret and clientid somewhere.
#' These credentials will be saved automatically to a standard filesystem
#' location if the user calls \code{store_cdse_credentials()}
#' with the \code{clientid} and \code{secret} parameters.
#' If the user chooses not to save credentials to the standard filesystem
#' location, then she will need to add both clientid and secret to each
#' \code{acquire_scihub()} function call.
#'
#' Using Credentials with \code{aquire_scihub}:
#' \itemize{
#'  \item If the credentials were stored using \code{store_cdse_credentials()}, the credentials are retrieved automatically.
#'  \item Otherwise, you can utilize the generated \code{clientid} and \code{secret} from \url{https://dataspace.copernicus.eu/} within the \code{aquire_scihub()} function.
#'  \item If you want to store your credentials on your computer, ensure that when running \code{aquire_scihub()}, the \code{save_creds} parameter is set to 'TRUE'.
#'  \item During the first run of \code{aquire_scihub()}, manually input your \code{clientid} and \code{secret} in the function signature. Subsequent runs will use the stored credentials.
#' }
#'
#' **Subject Area Constraint:**
#'  The downloadable images are restricted to a maximum size of 2500 pixels on
#'  each side. This limitation is established due to the final resolution set to
#'  10 meters using JavaScript. Consequently, the subject area available for
#'  download is limited to 25 kilometers in both directions. Please be aware of
#'  this restriction when selecting your desired area for download.

#' **Area of Interest (AOI) Specification:**
#'  When defining your Area of Interest (AOI), please ensure that it is
#'  represented as a polygonal layer with only one feature. This feature can
#'  either be a single POLYGON or a MULTIPOLYGON, which may consist of
#'  non-contiguous areas, but only one feature is permissible.
#'
#' @examples
#' \dontrun{
#' from_date <- "2018-12-01"
#' to_date <- "2019-04-30"
#' aoi <- sf::st_read(system.file("extdata",
#'                   "lachish.gpkg", package = 'rOPTRAM'))
#' optram_options("veg_index", "NDVI", show_opts = FALSE)
#' optram_options("SWIR_band", 11)
#' acquire_scihub(aoi, from_date, to_date,
#'              clientid = '...enter clientid here...',
#'              secret = '...enter secret here...')
#' }

acquire_scihub <- function(
    aoi,
    from_date, to_date,
    output_dir = tempdir(),
    save_creds = TRUE,
    clientid = NULL,
    secret = NULL) {

  # Package options
  SWIR_band <- getOption("optram.SWIR_band")
  veg_index <- getOption("optram.veg_index")
  max_cloud <- getOption("optram.max_cloud")
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

  # Make sure SWIR_band is one of 11 or 12
  if(length(SWIR_band) != 1 || !is.numeric(SWIR_band) ||
     !SWIR_band %in% c(11, 12)) {
    message("SWIR band must be either 11 or 12")
    return(NULL)
  }

  str_script <- paste0("STR", as.character(SWIR_band), ".js")
  vi_script <- paste0(veg_index, ".js")
  # Retrieve the necessary scripts
  script_file_boa <- system.file("scripts", "BOA.js", package = "rOPTRAM")
  script_file_str <- system.file("scripts", str_script, package = "rOPTRAM")
  script_file_vi <- system.file("scripts", vi_script, package = "rOPTRAM")

  img_list <- CDSE::SearchCatalog(aoi = aoi,
                                  from = from_date, to = to_date,
                                  collection = "sentinel-2-l2a",
                                  token = tok)
  # filter out cloud cover
  img_list <- img_list[img_list$tileCloudCover < max_cloud,]

  # If option "period" is set to "seasonal" apply SeasonFilter
  if (getOption("optram.period") == "seasonal") {
    img_list <- CDSE::SeasonalFilter(catalog = img_list,
                                     from = from_date,
                                     to = to_date)
  }

  # Retrieve the images in BOA,STR and VI formats
  get_result_list <- function(scrpt, s_dir){
    result_list <- lapply(seq_along(img_list), function(d){
        time_range <- as.character(img_list$acquisitionDate[d])
        sourceId <- img_list$sourceId[d]
        tileid <- unlist(strsplit(sourceId, split="_"))[6]
        result_rast <- CDSE::GetImage(aoi = aoi,
                                     time_range = time_range,
                                     script = scrpt,
                                     collection = "sentinel-2-l2a",
                                     format = "image/tiff",
                                     mask = TRUE,
                                     resolution = c(10,10),
                                     token = tok)

        raster_file <- file.path(s_dir,
                               paste0(basename(s_dir), "_",
                                      time_range, "_", tileid,
                                      ".tif"))
        terra::writeRaster(result_rast, raster_file, overwrite = TRUE)
        return(raster_file)
    })
    return(result_list)
  }

  result_boa <- get_result_list(script_file_boa, result_folder_boa)
  result_str <- get_result_list(script_file_str, result_folder_str)
  result_vi <- get_result_list(script_file_vi, result_folder_vi)

  return(result_boa)
}

#' @title Check access to scihub API
#' @description  The check_scihub function verifies the availability of a
#' CDSE API, retrieves credentials from a file or by provided clientid and
#' secret, obtains an OAuthClient through \code{\link[CDSE]{GetOAuthClient}}, and
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
  if (is.null(clientid) | is.null(secret)) {
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

#' @title Utility Function to Acquire Sentinel-2 Imagery using openEO
#' @description This non-exported function uses the \code{openeo} package
#' to send a request to Copernicus DataSpace, and prepare the products.
#' Called by \code{\link[rOPTRAM]{optram_acquire_s2}}
#' @param aoi, {sf} object, POLYGON or MULTIPOLYGON of area of interest
#' @param from_date, string, represents start of date range,
#'      formatted as "YYYY-MM-DD"
#' @param to_date, string, end of date range, formatted as "YYYY-MM-DD"
#' @param output_dir, string, path to save downloaded, and processed imagery
#' @param scale_factor, integer, scaling factor for EO data source default 10000
#' , to scale Sentinel-2 15 bit DN to range (0, 1)
#' @return list of BOA files
#' @export
#' @note
#' This function utilizes the \code{openeo} package.
#' Instructions for the login process:
#' First of all, to authenticate your account on the backend of the Copernicus
#' Data Space Ecosystem, it is necessary for you to complete the registration
#' process. Follow these instructions for registration:
#' \url{https://documentation.dataspace.copernicus.eu/Registration.html}
#' After you have registered and installed the \code{openeo} package, you can run the
#' \code{acquire_openeo} function.
#' During the process of connecting to the server and logging in, you need to
#' follow these steps:
#' \enumerate{
#'   \item When the message "Press <enter> to proceed:" appears in the console, press enter.
#'   \item When prompted with 'Copy CTGB-UGFU and paste when requested by the browser' in the console, it may appear but can be ignored, as it is related to an older version of the openeo package.
#'   \item Calling this method opens your system web browser, with which you can authenticate yourself on the back-end authentication system.
#'   \item After that, the website will give you instructions to go back to the R client, where your connection has logged your account in. This means that every call that comes after that via the connection variable is executed by your user account.
#'   \item You will be redirected to \url{https://identity.dataspace.copernicus.eu/}. Ensure you have an account and are logged in. You will be required to grant access - press "yes".
#'}
#'
#' @examples
#' \dontrun{
#' from_date <- "2018-12-01"
#' to_date <- "2019-04-30"
#' aoi <- sf::st_read(system.file("extdata",
#'                 "lachish.gpkg", package = 'rOPTRAM'))
#' Important! Assign the output to an object for later retrieval
#' acq <- acquire_openeo(aoi, from_date, to_date,
#'                       veg_index = "SAVI",
#'                       SWIR_band = 11)
#' }

acquire_openeo <- function(
    aoi,
    from_date, to_date,
    output_dir = tempdir(),
    scale_factor = 10000) {

  if(!check_openeo()) return(NULL)

  SWIR_band <- getOption("optram.SWIR_band")
  veg_index <- getOption("optram.veg_index")
  max_cloud <- getOption("optram.max_cloud")
  bbox <- sf::st_bbox(aoi)

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

  # Make sure SWIR_band is one of 11 or 12
  if(length(SWIR_band) != 1 || !is.numeric(SWIR_band) ||
     !SWIR_band %in% c(11, 12)) {
    message("SWIR band must be either 11 or 12")
    return(NULL)
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
    SWIR_band <- paste0("B", SWIR_band)
    SWIR_DN <- x[SWIR_band]
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

  result_vi <- p$save_result(data = cube_s2_vi, format = formats$output$GTiff,
                             options = list(filename_prefix=veg_index))
  result_str <- p$save_result(data = cube_s2_str, format = formats$output$GTiff,
                              options = list(filename_prefix="STR"))
  result_boa <- p$save_result(data = cube_S2_boa, format = formats$output$GTiff,
                              options = list(filename_prefix="BOA"))

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

  # Define a function to check job status
  check_all_statuses <- function(status_list) {
    for (status in status_list) {
      if (length(status) == 0) {
        message("One or more statuses are empty.")
        return(NULL)
      }
      if (status != "finished") {
        message("One or more jobs are not finished.")
        return(NULL)
      }
    }
    return(TRUE)  # All statuses are valid and finished
  }

  # Check all statuses
  all_statuses <- list(job_vi_status, job_str_status, job_boa_status)
  result <- check_all_statuses(all_statuses)

  if (is.null(result)) {
    message("One or more jobs failed to execute. Please check the logs on the
            OpenEO Web Editor.")
    return(NULL)
  }

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

  return(jobs_boa)
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
#' \itemize{
#'   \item When the message "Press <enter> to proceed:" appears in the console,
#' press enter.
#'   \item You will be redirected to \url{https://identity.dataspace.copernicus.eu/}.
#'   \item Ensure you have an account and are logged in. You will be required to grant access - press "yes".
#'   }

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
