#' @title Check access to Sentinel Hub, using sen2r
#' @description
#' Verify that sen2r is installed,
#' and check credentials for access to Sentinel Hub
#' @param scihub_user: string, scihub username
#' @param scihub_pass: string, scihub password
#' @param optram_func: string, which function called this check
#' 
#' (Not exported)
#' @return boolean: access to scihub using sen2r is possible
#' 

check_scihub_access <- function(scihub_user = NULL,
                                scihub_pass = NULL,
                                optram_func = "optram") {
    # Is sen2r installed?
    if (!require(sen2r)) {
            warning("This function requires the `sen2r` package.", "\n",
            "Please install that package first before running", optram_func)
            return(FALSE)
    }
    # Check for stored credentials (by default in "~/.sen2r/apihub.txt")
    if (!sen2r::is_scihub_configured()) {
        if (!is.null(scihub_user) && ! is.null(scihub_pass)){
        # If credentials have never preciously been been stored,
        # check the user supplied values, to be sure they are valid.
        if(sen2r::check_scihub_login(scihub_user, scihub_pass)) {
            sen2r_path <- file.path(path.expand("~"), ".sen2r")
            if (!dir.exists(sen2r_path)) {
                dir.create(sen2r_path)
            }
            # store valid credentials into "apihub.txt" for future
            apihub_path <- file.path(sen2r_path, "apihub.txt")
            sen2r::write_scihub_login(scihub_user, scihub_pass, apihub_path = apihub_path)
            return(TRUE)
        } else {
            warning("Login credentials not accepted by ESA Sentinel Hub",
            "\n", "Please verify Sentinel Hub login credentials.")
            return(FALSE)
        }
        } else {
            # No stored credentials, and no values passed in by user.
            # Print message and exit.
            warning("ESA Sentinel Hub requires authentication", "\n",
                    "Please register at:", "\n",
                    "https://scihub.copernicus.eu/userguide/SelfRegistration",
                    "then rerun function: ", optram_func, " specifying both:", "\n",
                    "scihub_user, and scihub_pass")
            return(FALSE)
        }
    }
    return(TRUE)
}