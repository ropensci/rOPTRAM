#' @title Prepare Dataframe with Pairs of NDVI and STR Values
#' @description Collect all pixel values of both vegetation index,
#' and Swir Transformed Reflectance, for a time series of images.
#' Prepare data.frame of all pairs of values
#' (as input for the `optram_wetdry_coefficients()` function)
#' @param STR_list, list of paths to STR raster files
#' @param VI_list, list of paths to NDVI raster files
#' @param output_dir, string, path to save data.frames (in RDS format)
#' @param max_tbl_size, numeric, maximum size of NDVI-STR data.frame
#'      default set to 5e+6
#' @param rm.low.vi, boolean, Should VI values <= 0 be removed
#'      default FALSE. If set to TRUE then all VI <= 0 will be set to NA.
#' @return full_df, data.frame with 5 columns: X,Y,Date,NDVI,STR
#' @export
#' @note
#' Use the `max_tbl_size` parameter to limit size of the NDVI-STR data.frame
#' With a large area of interest, and long time frame,
#' the number of data points can overrun the computation resources.
#' This parameter sets a total size of data.frame from the `max_tbl_size`
#' parameter, together with the number of image time slots in the time range.
#'
#' In some cases (i.e. water surfaces) NDVI can have values
#' below zero. These pixels can be removed from the trapezoid
#' by setting `rm.low.vi` to TRUE.
#'
#' The vegetation index column is named "NDVI" even though it can represent
#' other vegetation indices, such as SAVI, or MSAVI.
#'
#' @examples
#' VI_list <- list.files(system.file("extdata", "SAVI"),
#'         pattern = ".tif$", full.names = TRUE)
#' STR_list <- list.files(system.file("extdata", "STR"),
#'         pattern = ".tif$", full.names = TRUE)
#' full_df <- optram_ndvi_str(STR_list, VI_list)
#' # Show structure of output data.frame
#' str(full_df)

optram_ndvi_str <- function(STR_list, VI_list,
                            output_dir = tempdir(),
                            max_tbl_size = 5e+6,
                            rm.low.vi = FALSE){

  # Avoid "no visible binding for global variable" NOTE
  date_str <- STR <- STR_1_df <- STR_df <- STR_df_file <- NULL
  VI_df_list <- VI_df <- VI_df_1 <- size <- max_tbl <- NULL

  if (length(STR_list) == 0 || length(VI_list) == 0) {
    message("No raster files in directories")
    return(NULL)
  }
  if (!dir.exists(output_dir)) dir.create(output_dir)

  # Get index of rows for sampling
  # Use the first raster (first date) list of both STR rasters
  # and VI rasters to determine index for random sampling
  # Get minimum length of non-NA values from both rasters
  rs <- terra::rast(STR_list[1])
  rs_df <- terra::as.data.frame(rs, xy = TRUE, na.rm = TRUE)
  rv <- terra::rast(VI_list[1])
  rv_df <- terra::as.data.frame(rv, xy = TRUE, na.rm = TRUE)
  # Make sure length of data.frame is less than:
  # max_tbl_size / number of raster dates.
  # Other wise, take a sample
  nr <- min(nrow(rv_df), nrow(rs_df))
  if (nr > (max_tbl_size / length(STR_list))) {
    # Set sample size as:
    # maximum table / number of dates in date range
    samp_size <- max_tbl_size / length(STR_list)
    idx <- sample(nr, samp_size)
  } else {
    idx <- seq(1, nr)
  }

  df_list <- lapply(STR_list, function(f){
    # Get date from file name, and load STR raster
    date_str <- unlist(strsplit(basename(f), split = "_", fixed = TRUE))[2]
    STR <- terra::rast(f)
    # Also get the vegetation index raster for this date
    VI_f <- VI_list[grep(date_str, basename(VI_list))]
    VI <- terra::rast(VI_f)
    # Revert to original scale
    VI <- VI/10000.0

    # Convert to data.frames,
    # keep NA's so that number of rows in STR and VI still match
    STR_1_df <- terra::as.data.frame(STR, xy=TRUE, na.rm = FALSE)
    names(STR_1_df) <- c("x", "y", "STR")
    STR_1_df['Date'] <- as.Date(date_str, format="%Y%m%d")
    VI_1_df <- terra::as.data.frame(VI, xy=TRUE, na.rm = FALSE)
    names(VI_1_df) <- c("x", "y", "VI")
    # Apply rm.low.vi parameter
    if (rm.low.vi) {
       VI_1_df$VI[VI_1_df$VI <= 0.005]  <- NA
    }
    # Join two DF's
    df_1 <- dplyr::inner_join(VI_1_df, STR_1_df, by = c("x", "y"))
    # Remove NA and keep only sampled number of rows
    df_1 <- df_1[stats::complete.cases(df_1),]
    df_1 <- df_1[idx, ]
    return(df_1)
  })
  full_df <- do.call(rbind, df_list)

  # VI_df_list <- lapply(VI_list, function(f){
  #   # Get image date
  #   date_str <- unlist(strsplit(basename(f), split="_", fixed=TRUE))[2]
  #   VI <- terra::rast(f)
  #   # Revert to original scale
  #   VI <- VI/10000.0
  #
  #   VI_1_df <- terra::as.data.frame(VI, xy=TRUE, na.rm = FALSE)
  #   names(VI_1_df) <- c("x", "y", "VI")
  #
  #   # Apply rm.low.vi parameter
  #   if (rm.low.vi) {
  #     VI_1_df$VI[VI_1_df$VI <= 0.005]  <- NA
  #   }
  #   VI_1_df['Date'] <- as.Date(date_str, format="%Y%m%d")
  #   # Keep only sampled rows
  #   VI_1_df <- VI_1_df[idx, ]
  #   return(VI_1_df)
  # })
  # VI_df <- do.call(rbind, VI_df_list)

  df_file <- file.path(output_dir, "VI_STR_data.rds")
  saveRDS(full_df, df_file)
  message("Saved: ", nrow(full_df), " rows of VI-STR data to: ", df_file)

  return(full_df)
}
