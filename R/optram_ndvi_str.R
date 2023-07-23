#' @title Prepare dataframe of pairs NDVI and STR values
#' @description Collect all pixel values of both vegetation index,
#' and Swir Transformed Reflectance, for a time series of images.
#' Prepare data.frame of all pairs of values
#' (as input for the `optram_wetdry_coefficients()` function)
#' @param STR_list, list of paths to STR raster files
#' @param VI_list, list of paths to NDVI raster files
#' @param output_dir, string, path to save data.frames (in RDS format)
#' @param max_tbl_size, numeric, maximum size of NDVI-STR data.frame
#'      default set to 5e+6
#' @return full_df, data.frame with 5 columns: X,Y,Date,NDVI,STR
#' @export
#' @note
#' Use the `max_tbl_size` parameter to limit size of the NDVI-STR data.frame
#' With a large area of interest, and long time frame, 
#' the number of data points can overrun the computation resources.
#' This parameter sets a total size of data.frame from the `max_tbl_size` 
#' parameter, together with the number of image time slots in the time range.
#' @examples
#' print("Running optram_ndvi_str.R")

optram_ndvi_str <- function(STR_list, VI_list,
                            output_dir = tempdir(),
                            max_tbl_size = 5e+6){

  # Avoid "no visible binding for global variable" NOTE
  date_str <- STR <- STR_1_df <- STR_df <- STR_df_file <- NULL
  VI_df_list <- VI_df <- VI_df_1 <- NULL

  if (length(STR_list) == 0 || length(VI_list) == 0) {
    warning("No raster files in directories")
    return(NULL)
  }

  # Get index of rows for sampling
  r <- terra::rast(STR_list[1])
  r_df <- as.data.frame(r, xy = TRUE)
  if (nrow(r_df) > max_tbl-size) {
    samp_size <- max_tbl_size / length(STR_list)
    idx <- sample(nrow(r_df), samp_size)
  } else {
    idx <- seq(1, nrow(r_df))
  }

  STR_df_list <- lapply(STR_list, function(f){
    date_str <- unlist(strsplit(basename(f), split = "_", fixed = TRUE))[2]
    STR <- terra::rast(f)
    STR_1_df <- as.data.frame(STR, xy=TRUE)
    names(STR_1_df) <- c("x", "y", "STR")
    STR_1_df['Date'] <- as.Date(date_str, format="%Y%m%d")
    STR_1_df <- STR_1_df[idx, ]
    return(STR_1_df)
  })
  STR_df <- do.call(rbind, STR_df_list)

  VI_df_list <- lapply(VI_list, function(f){
    # Get image date
    date_str <- unlist(strsplit(basename(f), split="_", fixed=TRUE))[2]
    VI <- terra::rast(f)
    # Revert to original scale
    VI <- VI/10000.0
    VI_1_df <- as.data.frame(VI, xy=TRUE)
    names(VI_1_df) <- c("x", "y", "NDVI")
    VI_1_df['Date'] <- as.Date(date_str, format="%Y%m%d")
    VI_1_df <- VI_1_df[idx, ]
    return(VI_1_df)
  })
  VI_df <- do.call(rbind, VI_df_list)

  # Merge VI and STR pixel data
  full_df <- dplyr::full_join(STR_df, VI_df)
  full_df <- full_df[stats::complete.cases(full_df),]
  df_file <- file.path(output_dir, "VI_STR_data.rds")
  saveRDS(full_df, df_file)
  message("Saved: ", nrow(full_df), " rows of VI-STR data to: ", df_file)

  return(full_df)
}
