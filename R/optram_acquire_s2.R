#' @title Acquire Sentinel 2 Images at a Given Location and Date Range
#' @description Use the \code{CDSE} package to acquire, preprocess and crop
#'  Sentinel 2 satellite imagery.
#' @param aoi, {sf} POLYGON or MULTIPOLYGON, of boundary of area of interest
#' @param from_date, string, represents start of date range,
#'      formatted as "YYYY-MM-DD"
#' @param to_date, string, end of date range, formatted as "YYYY-MM-DD"
#' @param max_cloud, integer, maximum percent of cloud cover. Default 15.
#' @param output_dir, string, path to save processed imagery.
#' @param scale_factor, numeric, scale factor for reflectance values.
#'      Default 10000.
#' @param save_creds, logical, whether to save CDSE credentials. Default TRUE.
#' @param clientid, string, user's OAuth client id. Required if \code{save_creds}
#'      is TRUE.
#' @param secret, string, user's OAuth secret. Required if \code{save_creds} is TRUE.
#' @return output_path, string, path to downloaded files
#' @export
#' @note
#' This wrapper function calls one of multiple download functions,
#' each accessing a different cloud-based resource.
#' The cloud based resource can be one of "scihub" or "openeo":
#'
#' "scihub",...
#' If "scihub" then:
#' This function utilizes the \code{CDSE} package.
#' Make sure to install the \code{CDSE} and \code{jsonlite} packages.
#' Create OAuth account and token:
#' Creating an Account:
#' \enumerate{
#'  \item Navigate to the \href{https://dataspace.copernicus.eu/}{Copernicus portal}.
#'  \item Click the "Register" button to access the account creation page.
#'  \item If already registered, enter your username and password, and click "Login."
#'  \item Once logged in, go to the User dashboard and click "User Settings" to access the Settings page.
#' }
#' Creating OAuth Client:
#' \enumerate{
#'  \item On the Settings page, click the green "Create New" button located on the right.
#'  \item Enter a suitable "Client Name" and click the green "Create Client" button.
#'  \item A Client secret is generated.
#' }
#' the user must save her secret and clientid somewhere.
#' these credentials will be saved automatically to a standard filesystem
#' location if the user calls \code{check_scihub()} with the argument save_creds
#' set to TRUE (recommended).
#' if the user chooses not to save credentials to the standard filesystem
#' location, then she will need to add both clientid and secret to each
#' \code{acquire_scihub()} function call.
#'
#' Using Credentials with \code{aquire_scihub()}:
#' \itemize{
#'  \item Now, you can utilize the generated \code{clientid} and \code{secret} in
#'    the \code{aquire_scihub()} function.
#'  \item If you want to store your credentials on your computer, ensure that when running \code{aquire_scihub}, the \code{save_creds} parameter is set to `TRUE`.
#'  \item During the first run of \code{aquire_scihub()}, manually input your \code{clientid} and \code{secret} in the function's signature.
#'  \item Subsequent runs will use the stored credentials.
#'}
#'
#' Using Credentials with \code{aquire_scihub()}:
#' \itemize{
#'  \item Now, you can utilize the generated \code{clientid} and \code{secret} in
#'    the \code{aquire_scihub()} function.
#'  \item If you want to store your credentials on your computer, ensure that when running \code{aquire_scihub}, the \code{save_creds} parameter is set to TRUE.
#'  \item During the first run of \code{aquire_scihub()}, manually input your \code{clientid} and \code{secret} in the function's signature.
#'  \item Subsequent runs will use the stored credentials.
#'}
#'
#' Subject Area Constraint:
#'  The downloadable images are restricted to a maximum size of 2500 pixels on
#'  each side. This limitation is established due to the final resolution set to
#'  10 meters using JavaScript. Consequently, the subject area available for
#'  download is limited to 25 kilometers in both directions. Please be aware of
#'  this restriction when selecting your desired area for download.

#' Area of Interest (AOI) Specification:
#'  When defining your Area of Interest (AOI), please ensure that it is
#'  represented as a POLYGON or MULTIPOLYGON layer.
#'
#' "openeo",...
#' If "openeo" then:
#' This function utilizes the \code{openeo} package.
#' Instructions for the login process:
#' First of all, to authenticate your account on the backend of the Copernicus
#' Data Space Ecosystem, it is necessary for you to complete the registration
#' process. Follow these instructions for registration:
#' \url{https://documentation.dataspace.copernicus.eu/Registration.html}
#' After you have registered and installed the \code{openeo} package, you can run the
#' \code{acquire_openeo()} function.
#' During the process of connecting to the server and logging in, you need to
#' follow these steps:
#' \enumerate{
#'   \item When the message "Press <enter> to proceed:" appears in the console, press enter.
#'   \item Calling this method opens your system web browser, with which you can authenticate yourself on the back-end authentication system. After that, the website will give you instructions to go back to the R client, where your connection has logged your account in. This means that every call that comes after that via the connection variable is executed by your user account.
#'   \item You will be redirected to \url{https://identity.dataspace.copernicus.eu/}. Ensure you have an account and are logged in. You will be required to grant access - press "yes".
#'}
#'
#' Two SWIR bands are available in Sentinel-2: 1610 nanometer (nm) and 2190 nm.
#' Setting the option \code{SWIR_bands} with \code{\link[rOPTRAM]{optram_options}} allows to choose which band is used in this model.
#'
#' @examples
#' \dontrun{
#' from_date <- "2018-12-01"
#' to_date <- "2019-04-30"
#' aoi <- sf::st_read(system.file("extdata",
#'                               "lachish.gpkg", package = 'rOPTRAM'))
#' s2_file_list <- optram_acquire_s2(aoi,
#'                                  from_date, to_date,
#'                                  remote = "scihub",
#'                                  veg_index = "SAVI",
#'                                  SWIR_band = 11)
#' }

optram_acquire_s2 <- function(
      aoi,
      from_date, to_date,
      max_cloud = 10,
      output_dir = tempdir(),
      scale_factor = 10000,
      save_creds = TRUE,
      clientid = NULL,
      secret = NULL) {
  # Avoid "no visible binding for global variable" NOTE
  scihub <- openeo <- NULL

  # Get the SWIR_band and remote from package options
  remote <- getOption("optram.remote")
  SWIR_band <- getOption("optram.SWIR_band")
  # Pre flight checks...
  if (!check_aoi(aoi)) return(NULL)
  # Ensure that aoi is single POLYGON or MULTIPOLYGON feature (and sf NOT sfc)
  aoi <- sf::st_as_sf(sf::st_union(aoi))
  if (!check_date_string(from_date, to_date)) return(NULL)
  if (!check_swir_band(SWIR_band)) return(NULL)

  switch(remote,
         scihub = acquire_scihub(aoi = aoi,
                                 from_date = from_date, to_date = to_date,
                                 max_cloud = max_cloud,
                                 output_dir = output_dir,
                                 save_creds = save_creds,
                                 clientid = clientid,
                                 secret = secret),
         openeo = acquire_openeo(aoi = aoi,
                                 from_date = from_date, to_date = to_date,
                                 max_cloud = max_cloud,
                                 output_dir = output_dir,
                                 scale_factor = scale_factor)
         )
}
