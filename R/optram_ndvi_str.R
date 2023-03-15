#' @title Prepare dataframe of pairs NDVI and STR values
#'
#' @description Collect all pixel values of both vegetation index,
#' and Swir Transformed Reflectance, for a time series of images.
#' Prepare data.frame of all pairs of values
#' (as input for the `optram_wetdry_coefficients()` function)
#'
#' @param STR_list: list of paths to STR raster files
#' @param VI_list: list of paths to NDVI raster files
#' @param output_dir: string, path to save data.frames (in RDS format)
#' @return full_df, data.frame
#' @export
#'
#' @examples
#' print("Running optram_ndvi_str.R")
#'
optram_ndvi_str <- function(STR_list, VI_list,
                            output_dir = tempdir()){

  # Avoid "no visible binding for global variable" NOTE
  date_str <- STR <- STR_1_df <- STR_df <- STR_df_file <- NULL
  VI_df_list <- VI_df <- VI_df_1 <- NULL
  
  STR_df_list <- lapply(STR_list, function(f){
    date_str <- unlist(strsplit(basename(f), split = "_", fixed = TRUE))[2]
    STR <- terra::rast(f)
    STR_1_df <- as.data.frame(STR, xy=TRUE)
    names(STR_1_df) <- c("x", "y", "STR")
    STR_1_df['Date'] <- as.Date(date_str, format="%Y%m%d")
    return(STR_1_df)
  })
  STR_df <- do.call(rbind, STR_df_list)
  STR_df_file <- file.path(output_dir, "STR_data.rds")
  saveRDS(STR_df, STR_df_file)
  message("Saved: ", nrow(STR_df), " rows to: ", STR_df_file)

  VI_df_list <- lapply(VI_list, function(f){
    # Get image date
    date_str <- unlist(strsplit(basename(f), split="_", fixed=TRUE))[2]
    VI <- terra::rast(f)
    # Revert to original scale
    VI <- VI/10000.0
    VI_1_df <- as.data.frame(VI, xy=TRUE)
    names(VI_1_df) <- c("x", "y", "NDVI")
    VI_1_df['Date'] <- as.Date(date_str, format="%Y%m%d")
    return(VI_1_df)
  })
  VI_df <- do.call(rbind, VI_df_list)
  VI_df_file <- file.path(output_dir, "VI_data.rds")
  saveRDS(VI_df, VI_df_file)
  message("Saved: ", nrow(VI_df), " rows to: ", VI_df_file)

  full_df <- dplyr::full_join(STR_df, VI_df)
  full_df <- full_df[stats::complete.cases(full_df),]
  message("Full data joined leaving: ", nrow(full_df), " rows")
  return(full_df)
}
