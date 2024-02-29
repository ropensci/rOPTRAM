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
#' @param rm.hi.str, boolean, Should STR values > 1.5*IQR (outliers) be removed
#'      default FALSE. If set to TRUE then outlier STR values will be set to NA.
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
                            rm.low.vi = FALSE,
                            rm.hi.str = FALSE){

  # Avoid "no visible binding for global variable" NOTE
  date_str <- STR <- STR_1_df <- STR_df_file <- VI_df_list <- VI_df_1 <- NULL

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
    # Read STR raster and convert to data.frame,
    STR <- terra::rast(f)
    # keep NA's so that number of rows in STR and VI stay synchronized
    STR_1_df <- terra::as.data.frame(STR, xy=TRUE, na.rm = FALSE)
    names(STR_1_df) <- c("x", "y", "STR")
    if (rm.hi.str) {
      # Calculate inter quartile range, and set all STR values
      # above (1.5 * IQR) to NA
      STR_q <- stats::quantile(STR_1_df, probs = c(0.25, 0.75), na.rm = TRUE)
      STR_IQR <- STR_q[2] - STR_q[1]
      STR_1_df$STR[STR_1_df$STR >= STR_IQR*1.5] <- NA
    }

    # Also get the vegetation index raster for this date
    bn <- gsub(".tif", "", basename(f))
    date_str <- unlist(strsplit(bn, split = "_", fixed = TRUE))[2]
    VI_f <- VI_list[grep(date_str, basename(VI_list))]
    if (length(VI_f) == 0) { return(NULL) }
    else if (!file.exists(VI_f)) { return(NULL) }

    VI <- terra::rast(VI_f)
    # Revert to original scale
    # VI <- VI/10000.0  **NOT necessary in CDSE!**
    VI_1_df <- terra::as.data.frame(VI, xy=TRUE, na.rm = FALSE)
    names(VI_1_df) <- c("x", "y", "VI")

    if (rm.low.vi) {
      # Apply rm.low.vi parameter, set VI to NA when values <= 0.005
       VI_1_df$VI[VI_1_df$VI <= 0.005]  <- NA
    }
    # Join two DF's
    df_1 <- dplyr::inner_join(VI_1_df, STR_1_df, by = c("x", "y"))
    # Remove NA and keep only sampled number of rows
    df_1 <- df_1[stats::complete.cases(df_1),]
    df_1 <- df_1[idx, ]

    # Use date from file name, and add Date column
    df_1['Date'] <- as.Date(date_str, format="%Y-%m-%d")

    return(df_1)
  })
  full_df <- do.call(rbind, df_list)

  df_file <- file.path(output_dir, "VI_STR_data.rds")
  saveRDS(full_df, df_file)
  message("Saved: ", nrow(full_df), " rows of VI-STR data to: ", df_file)

  return(full_df)
}
