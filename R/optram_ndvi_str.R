#' @title Prepare Dataframe with Pairs of NDVI and STR Values
#' @description Collect all pixel values of both vegetation index,
#' and Swir Transformed Reflectance, for a time series of images.
#' Prepare data.frame of all pairs of values
#' (as input for the \code{\link[rOPTRAM]{optram_wetdry_coefficients}} function)
#' @param STR_list, list of paths to STR raster files
#' @param VI_list, list of paths to NDVI raster files
#' @param output_dir, string, path to save data.frames (in RDS format)
#' @param aoi, {sf} POLYGON or MULTIPOLYGON,
#'  must have a numeric column named "ID" for coloring trapezoid points by features
#'  Default NULL, (no coloring)
#' @return full_df, data.frame with 5 columns: X,Y,Date,NDVI,STR
#' @export
#' @note
#' Use the option \code{max_tbl_size} (see \code{\link[rOPTRAM]{optram_options}})
#' to limit size of the NDVI-STR data.frame.
#' With a large area of interest, and long time frame,
#' the number of data points can overrun the computation resources.
#' This parameter sets a total size of data.frame from the \code{max_tbl_size}
#' parameter, together with the number of image time slots in the time range.
#'
#' In some cases (i.e. water surfaces) NDVI can have values
#' below zero. These pixels can be removed from the trapezoid
#' by setting \code{rm.low.vi}
#' option to TRUE.
#'
#' The vegetation index column is named "NDVI" even though it can represent
#' other vegetation indices, such as SAVI, or MSAVI.
#'
#' @examples
#' VI_list <- list.files(system.file("extdata", "NDVI"),
#'         pattern = ".tif$", full.names = TRUE)
#' STR_list <- list.files(system.file("extdata", "STR"),
#'         pattern = ".tif$", full.names = TRUE)
#' full_df <- optram_ndvi_str(STR_list, VI_list)
#' # Show structure of output data.frame
#' str(full_df)

optram_ndvi_str <- function(STR_list, VI_list,
                            output_dir = tempdir(),
                            aoi = NULL){

  # Avoid "no visible binding for global variable" NOTE
  date_str <- STR <- STR_1_df <- STR_df_file <- VI_df_list <- VI_df_1 <- NULL

  if (length(STR_list) == 0 || length(VI_list) == 0) {
    message("No raster files in directories")
    return(NULL)
  }
  if (!dir.exists(output_dir)) dir.create(output_dir)

  max_tbl_size <- getOption("optram.max_tbl_size")
  rm.low.vi <- getOption("optram.rm.low.vi")
  rm.hi.str <- getOption("optram.rm.hi.str")

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

    # In case coloring by features is requested, transform the AOI into raster
    # and get ID values to add to the VI_STR data.frame
    feature_col <- getOption("optram.feature_col")
    if ((getOption("optram.plot_colors") %in% c("features", "feature")) &
        (!is.null(aoi)) &
        (feature_col %in% names(aoi))) {
      aoi_rast <- terra::rasterize(x = aoi, y = STR,
                                   field = feature_col, touches = TRUE)
      ID_df <- terra::as.data.frame(aoi_rast, xy = TRUE, na.rm = FALSE)
      names(ID_df) <- c("x", "y", "Feature_ID")
      STR_1_df <- dplyr::inner_join(STR_1_df, ID_df,
                                    by = c("x", "y"), keep = FALSE)
    }

    # Also get the vegetation index raster for this date/tileid
    unique_str <- gsub("STR_", "", basename(f))
    VI_f <- VI_list[grep(unique_str, basename(VI_list))]
    if (length(VI_f) == 0) {
      return(NULL)
    } else if (all(!file.exists(VI_f))) {
      return(NULL)}

    VI <- terra::rast(VI_f)
    # Revert to original scale
    # VI <- VI/10000.0  **NOT necessary in CDSE!**
    VI_1_df <- terra::as.data.frame(VI, xy=TRUE, na.rm = FALSE)
    names(VI_1_df) <- c("x", "y", "VI")

    if (rm.low.vi) {
      # Apply rm.low.vi parameter, set VI to NA when values <= 0.005
       VI_1_df$VI[VI_1_df$VI <= 0.005]  <- NA
    }
    # Join two DF's and keep only sampled number of rows
    df_1 <- dplyr::inner_join(VI_1_df, STR_1_df,
                              by = c("x", "y"), keep = FALSE)
    df_1 <- df_1[idx, ]

    # Get date and tileid from file name, and add to data.frame
    date_tile <- unlist(strsplit(gsub(".tif", "", unique_str), "_"))
    df_1['Date'] <- as.Date(date_tile[1], format="%Y-%m-%d")
    df_1['Tile'] <- date_tile[2]
    # Remove rows with NA in VI or STR columns
    df_1 <- df_1[stats::complete.cases(df_1[3:4]),]
    return(df_1)
  })
  full_df <- do.call(rbind, df_list)
  # Unnecessary to remove NA again
  # full_df <- full_df[stats::complete.cases(full_df),]

  # Calculate point density and add Density column
  # (Using: https://slowkow.com/notes/ggplot2-color-by-density/ )
  get_density <- function(x, y, ...) {
    dens <- MASS::kde2d(x, y, ...)
    ix <- findInterval(x, dens$x)
    iy <- findInterval(y, dens$y)
    ii <- cbind(ix, iy)
    return(dens$z[ii])
  }
  full_df$Density <- get_density(full_df$VI, full_df$STR, n = 128)
  #full_df$Density <- log(full_df$Density)

  df_file <- file.path(output_dir, "VI_STR_data.rds")
  saveRDS(full_df, df_file)
  message("Saved: ", nrow(full_df), " rows of VI-STR data to: ", df_file)

  return(full_df)
}
