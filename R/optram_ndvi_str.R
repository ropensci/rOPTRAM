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
#' @return full_df, data.frame with 7 columns: X,Y,Date,Month,NDVI,STR,Density
#'  and optionally a 7th column with feature ID values from the AOI polygon.
#'  The columns Month, Density, Feature_ID  can be used in
#'  \code{plot_vi_str_cloud()} function to color the points in the scatter plot
#'  in various ways.
#' @export
#' @note
#' Use the option \code{max_tbl_size} (see \code{\link[rOPTRAM]{optram_options}})
#' to limit size of the NDVI-STR data.frame.
#' With a large area of interest, and long time frame,
#' the number of data points can overrun the computation resources.
#' This parameter sets a total size of data.frame from the \code{max_tbl_size}
#' parameter, together with the number of image time slots in the time range.
#'
#' All rows in the data.frame where either NDVI or STR are NA are removed.
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
#' VI_list <- list.files(system.file("extdata", "NDVI", package = "rOPTRAM"),
#'         pattern = ".tif$", full.names = TRUE)
#' STR_list <- list.files(system.file("extdata", "STR", package = "rOPTRAM"),
#'         pattern = ".tif$", full.names = TRUE)
#' full_df <- optram_ndvi_str(STR_list, VI_list)
#' # Show structure of output data.frame
#' str(full_df)

optram_ndvi_str <- function(STR_list, VI_list,
                            output_dir = tempdir(),
                            aoi = NULL){

  # Avoid "no visible binding for global variable" NOTE
  date_str <- STR <- STR_1_df <- STR_df_file <- NULL
  ID_df <- VI_df_list <- VI_df_1 <- NULL

  if (length(STR_list) == 0 || length(VI_list) == 0) {
    message("No raster files in directories")
    return(NULL)
  }
  if (!dir.exists(output_dir)) dir.create(output_dir)

  max_tbl_size <- getOption("optram.max_tbl_size")
  rm.low.vi <- getOption("optram.rm.low.vi")
  rm.hi.str <- getOption("optram.rm.hi.str")

  # In case coloring by features is requested, transform the AOI into raster
  # and get ID values to add to the VI_STR data.frame
  feature_col <- getOption("optram.feature_col")
  if ((getOption("optram.plot_colors") %in% c("features", "feature")) &
      (!is.null(aoi)) &
      (feature_col %in% names(aoi))) {
    aoi_rast <- terra::rasterize(x = terra::vect(aoi),
                                 y = terra::rast(STR_list[1]),
                                 field = feature_col, touches = TRUE)
    ID_df <- terra::as.data.frame(aoi_rast, xy = TRUE, na.rm = FALSE)
    names(ID_df) <- c("x", "y", "Feature_ID")
  }

  df_list <- lapply(STR_list, function(f){
    # Read STR raster and convert to data.frame,
    STR <- terra::rast(f)

    # Also get the vegetation index raster for this date/tileid
    unique_str <- gsub("STR_", "", basename(f))
    VI_f <- VI_list[grep(unique_str, basename(VI_list))]
    if (length(VI_f) == 0) {
      return(NULL)
    } else if (all(!file.exists(VI_f))) {
      return(NULL)
      }
    VI <- terra::rast(VI_f)

    # If either raster is totally empty, return NULL
    if ( ! (terra::global(VI, "anynotNA") &
            terra::global(STR, "anynotNA") ) )  {
      return(NULL)
    }

    # Now get pixel values from both
    # keep NA's so that number of rows in STR and VI stay synchronized
    STR_1_df <- terra::as.data.frame(STR, xy=TRUE, na.rm = FALSE)
    names(STR_1_df) <- c("x", "y", "STR")

    VI_1_df <- terra::as.data.frame(VI, xy=TRUE, na.rm = FALSE)
    names(VI_1_df) <- c("x", "y", "VI")

    # Join two DF's and keep only sampled number of rows
    df_1 <- dplyr::inner_join(VI_1_df, STR_1_df,
                              by = c("x", "y"), keep = FALSE)
    # Remove low VI values
    if (rm.low.vi) {
      # Apply rm.low.vi parameter, set VI to NA when values <= 0.005
      df_1$VI[df_1$VI <= 0.005]  <- NA
    }
    # Add the Feature_ID if it exists
    if (!is.null(ID_df)) {
      STR_1_df <- dplyr::inner_join(STR_1_df, ID_df,
                                    by = c("x", "y"), keep = FALSE)
    }

    # Remove rows with NA in VI or STR columns
    df_1 <- df_1[stats::complete.cases(df_1[3:4]),]
    # Make sure some rows are left
    if (nrow(df_1) == 0) {
      return(NULL)
    }

    # Now make sure we are below the max_tbl_size
    max_size_1 <- max_tbl_size / length(STR_list)
    if (nrow(df_1) > max_size_1) {
      # Set sample size as:
      # maximum table / number of dates in date range
      idx <- sample(nrow(df_1), max_size_1)
      df_1 <- df_1[idx,]
    }

    # Get date and tileid from file name, and add to data.frame
    date_tile <- unlist(strsplit(gsub(".tif", "", unique_str), "_"))
    # After strsplit:
    #   first string is date, second is the time UTC, and third is tileid
    df_1['TimestampUTC'] <- as.POSIXct(paste(date_tile[1], date_tile[2]),
                                    format="%Y-%m-%d %H-%M-%S", tz="UTC")
    df_1['Tile'] <- date_tile[3]
    # Get month, for plotting "season trajectory" option in scatterplot
    df_1['Month'] <- format(df_1$Timestamp, "%m")

    return(df_1)
  })
  full_df <- do.call(rbind, df_list)

  # Now, remove high STR values
  # This has to be outside the lapply loop in oder to use the whole DF
  # to calculate quartiles
  if (rm.hi.str) {
    # Calculate inter quartile range, and set all STR values
    # above (1.5 * IQR) to NA
    # message("Applying rm.hi.str")
    Q3 <- stats::quantile(full_df$STR, probs = 0.75, na.rm = TRUE)
    STR_IQR <- stats::IQR(full_df$STR, na.rm = TRUE)
    cutoff <- Q3 + 1.5 * STR_IQR
    full_df$STR[full_df$STR >= cutoff] <- NA
  }
  # and Remove those rows
  full_df <- full_df[stats::complete.cases(full_df[3:4]),]

  df_file <- file.path(output_dir, "VI_STR_data.rds")
  saveRDS(full_df, df_file)
  message("Saved: ", nrow(full_df), " rows of VI-STR data to: ", df_file)

  return(full_df)
}
