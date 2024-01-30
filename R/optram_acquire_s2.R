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
#' @param output_dir, string, path to save processed imagery.
#' @param veg_index, string, which index to prepare. Default "NDVI".
#'  Can be "NDVI", "SAVI", "MSAVI", etc
#' @param remove_safe, string, "yes" or "no":
#'      whether to delete downloaded SAFE directories
#'      after processing, default "yes"
#' @param safe_dir, string, path to save downloaded Sentinel imagery,
#'      If `remove_safe` is "no" then SAFE directories will be saved here.
#'      Default is `output_dir`, the same directory as processed images.
#' @param scale_factor, numeric, scale factor for reflectance values. 
#'      Default 10000.
#' @param save_creds, logical, whether to save CDSE credentials. Default TRUE.
#' @param clientid, string, user's OAuth client id. Required if `save_creds` 
#'      is TRUE.
#' @param secret, string, user's OAuth secret. Required if `save_creds` is TRUE.
#' @param remote, string, from which archive to download imagery
#'    possible values: 'gcloud', 'scihub', 'openeo'
#' @return output_path, string, path to downloaded files
#' @export
#' @note
#' This wrapper function calls one of multiple download functions,
#' each accessing a different cloud-based resource.
#' The cloud based resource can be one of:
#' 
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
#' "scihub",...
#' If "scihub" then: 
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
#'    
#' "openeo",...
#' If "openeo" then: 
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
#' aoi <- system.file("extdata", "migda.gpkg", package = 'rOPTRAM')
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
      safe_dir = output_dir,
      veg_index = "NDVI",
      scale_factor = 10000,
      save_creds = TRUE,
      clientid = NULL,
      secret = NULL,
      remote = c("gcloud", "scihub", "openeo")) {
  # Avoid "no visible binding for global variable" NOTE
  gcloud <- scihub <- openeo <- NULL

  # Pre flight checks...
  if (!check_aoi(aoi_file)) return(NULL)
  if (!check_date_string(from_date, to_date)) return(NULL)

  remote <- match.arg(remote)

  switch(remote,
         gcloud = acquire_gcloud(aoi_file = aoi_file,
                                 from_date = from_date, to_date = to_date,
                                 max_cloud = max_cloud,
                                 timeperiod = timeperiod,
                                 output_dir = output_dir,
                                 remove_safe = remove_safe,
                                 safe_dir = safe_dir,
                                 veg_index = veg_index),
         scihub = acquire_scihub(aoi_file = aoi_file,
                                 from_date = from_date, to_date = to_date,
                                 max_cloud = max_cloud,
                                 timeperiod = timeperiod,
                                 output_dir = output_dir,
                                 remove_safe = remove_safe,
                                 safe_dir = safe_dir,
                                 veg_index = veg_index,
                                 save_creds = save_creds,
                                 clientid = clientid,
                                 secret = secret),
         openeo = acquire_openeo(aoi_file = aoi_file,
                                 from_date = from_date, to_date = to_date,
                                 max_cloud = max_cloud,
                                 timeperiod = timeperiod,
                                 output_dir = output_dir,
                                 remove_safe = remove_safe,
                                 safe_dir = safe_dir,
                                 veg_index = veg_index,
                                 scale_factor = scale_factor)
         )
}
