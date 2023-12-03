#' @title Derive Coefficients of Slope and Intercept
#' @description Derive slope and intercept coefficients
#' for both wet and dry trapezoid lines.
#' Write coefficients to a CSV file
#' (as input to `optram_soilmoisture()` function)
#' @param full_df, data.frame of STR and NDVI values
#' @param output_dir, string, directory to save coefficients CSV file
#' @param step, float
#' @param aoi_file, string, added to title of plot
#'  (Can be path to AOI file, then the file name is used in plot title)
#' @param save_plot, boolean, If TRUE (default) save scatterplot to output_dir
#' @return coeffs, list of float, coefficients of wet-dry trapezoid
#' @export
#' @note
#' The vegetation index column is named "VI" though it can represent
#' several vegetation indices, such as SAVI, or MSAVI.
#' @examples
#' aoi_file <- "Test"
#' full_df <- readRDS(system.file("extdata",
#'               "VI_STR_data.rds",
#'               package = "rOPTRAM"))
#' coeffs <- optram_wetdry_coefficients(full_df, aoi_file)
#' print(coeffs)

optram_wetdry_coefficients <- function(full_df,
                                       aoi_file,
                                       output_dir = tempdir(),
                                       step = 0.01,
                                       save_plot = TRUE) {
  # Derive slope and intercept to two sides of trapezoid
  # Based on:
  # https://github.com/teerathrai/OPTRAM
  #
  # Parameters:
  #   full_df: data.frame, table of NDVI and STR values
  # Returns:
  #   coeffs: array of floats, slope & intercept for both wet and dry lines

  # Create series of values for NDVI
  # Get min/max values from NDVI data, slightly smaller than full range

  # Avoid "no visible binding for global variable" NOTE
  VI_min_max <- VI_series <- VI_STR_list <- VI_STR_df <- NULL
  Qs <- str_max <- str_min <- interval_df <- VI_STR_df1 <- NULL

  #  Pre-flight Check
  if (is.null(aoi_file)) {
    aoi_name <- NULL
    } else if (!file.exists(aoi_file)) {
      aoi_name <- aoi_file
      } else {aoi_name <- rOPTRAM:::aoi_to_name(aoi_file)}

  # Make sure no Inf or NA in full_df
  full_df <- full_df[is.finite(full_df$VI), ]
  VI_min_max <- round(stats::quantile(full_df$VI, c(0.1, 0.98)), 2)
  VI_series <- seq(VI_min_max[[1]], VI_min_max[[2]], step)
  message("VI series length:", length(VI_series))
  VI_STR_list <- lapply(VI_series, function(i){
    # Set NDVI value at midpoint of each interval
    vi_val <- i + step/2.0

    # Subset the data.frame to include only NDVI values between i and i+step
    interval_df <-  full_df[full_df$VI>=i & full_df$VI < (i+step),]
    # if too few rows in this interval, skip it, just return NULL
    if (nrow(interval_df) < 4) {
      return(NA)
    }
    # Remove lower than 2% and more than 98% quartile of STR values
    Qs <- stats::quantile(interval_df$STR, c(0.01, 0.99), na.rm=TRUE)
    interval_df <- interval_df[interval_df$STR<=Qs[[2]] &
                                 interval_df$STR>=Qs[[1]],]
    # Now, with outliers removed, find min (dry) and max (wet)
    # Within each interval
    str_max <- max(interval_df$STR, na.rm = TRUE)
    str_min <- min(interval_df$STR, na.rm = TRUE)
    VI_STR_df1 <- data.frame("VI" = vi_val,
                               "STR_wet" = str_max,
                               "STR_dry" = str_min)
    return(VI_STR_df1)
  })
  # Bind all interval results into one long DF
  VI_STR_list <- VI_STR_list[ !is.na(VI_STR_list) ]
  VI_STR_df <- do.call(rbind, VI_STR_list)

  # Save STR and VI values to CSV
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  utils::write.csv(VI_STR_df,
            file.path(output_dir, "VI_STR_df.csv"),
            row.names = FALSE)
  # Run linear regression between STR and NDVI
  # to determine the intercept and slope for both wet and dry data
  wet_fit <- stats::lm(STR_wet ~ VI, data=VI_STR_df)
  dry_fit <- stats::lm(STR_dry ~ VI, data=VI_STR_df)
  i_wet <- wet_fit$coefficients[[1]]
  s_wet <- wet_fit$coefficients[[2]]
  i_dry <- dry_fit$coefficients[[1]]
  s_dry <- dry_fit$coefficients[[2]]
  coeffs <- data.frame("intercept_dry"=i_dry, "slope_dry"=s_dry,
                       "intercept_wet"=i_wet, "slope_wet"=s_wet)
  utils::write.csv(coeffs,
            file.path(output_dir, "coefficients.csv"),
            row.names=FALSE)

  if (save_plot) {
    rOPTRAM::plot_ndvi_str_cloud(full_df,
                                coeffs,
                                aoi_name,
                                output_dir = output_dir)
  }
  return(coeffs)
}


#' @title Create Scatter Plot of STR-NDVI Data Points,
#' @description
#' Plot STR-NDVI scatterplot to show dry and wet trapezoid lines
#' over scatterplot of multi-temporal STR and NDVI pixel values
#' @param full_df, data.frame of NDVI and STR pixel values
#' @param coeffs, list of floats, the slope and intercept
#'   of wet and dry regression lines
#' @param aoi_name, string, used in plot title
#' @param output_dir, string, directory to save plot png file.
#' @return None
#' @export
#' @import ggplot2
#' @examples
#' aoi_name <- "Test"
#' full_df <- readRDS(system.file("extdata", "VI_STR_data.rds",
#'         package = "rOPTRAM"))
#' coeffs <- read.csv(system.file("extdata", "coefficients.csv",
#'         package = "rOPTRAM"))
#' plot_ndvi_str_cloud(full_df, coeffs, aoi_name)

plot_ndvi_str_cloud <- function(full_df,
                                coeffs,
                                aoi_name,
                                output_dir = tempdir()) {
  # Avoid "no visible binding for global variable" NOTE
  i_dry <- i_wet <- s_dry <- s_wet <- plot_df <- plot_path <- NULL
  x_min <- x_max <- y_min <- y_max <- VI_STR_df1 <- VI <- STR <- NULL

  # Pre-flight test
  if (!ncol(coeffs) == 4) {
    message("Coefficients not correctly formed. \n
    Be sure the CSV file has 4 columns. Exiting...")
    return(NULL)
  }
  if (! "STR" %in% names(full_df)) {
    message("STR column missing from data.frame. Exiting...")
    return(NULL)
  }
  if (! "VI" %in% names(full_df)) {
    message("VI column missing from data.frame. Exiting...")
    return(NULL)
  }

  i_dry <- round(coeffs$intercept_dry, 3)
  s_dry <- round(coeffs$slope_dry, 3)
  i_wet <- round(coeffs$intercept_wet, 3)
  s_wet <- round(coeffs$slope_wet, 3)

  # We don't need Millions of points! get a subset
  num_rows <- nrow(full_df)
  if (num_rows < 400000) {
    plot_df <- full_df
  } else {
    # This trick drops the num of plotted points by orders of magnitude
    samp_num <- num_rows  / log2(num_rows)
    sample_idx <- sample(num_rows, samp_num)
    plot_df <- full_df[sample_idx, ]
  }
  num_rows_plotted <- nrow(plot_df)
  # NDVI (x) axis limits
  # Set fixed plot limits
  x_min <- 0.0
  x_max <- 0.9
  # STR (y) axis limits
  y_min <- 0.1
  #y_max <- 3.6
  y_max <- max(plot_df$STR)*1.1
  # Text to add to plot
  coeffs_text <- paste("Dry intercept:", i_dry, "\n Dry slope:", s_dry,
                       "\n Wet intercept:", i_wet, "\n Wet slope:", s_wet)
  ggplot2::ggplot(plot_df) +
    geom_point(aes(x=VI, y=STR),
               color = "#0070000b", alpha = 0.3, size = 0.2) +
    # Wet edge
    geom_abline(intercept = i_wet, slope = s_wet,
                color = "#2E94B9", linewidth = 1.0) +
    # Dry edge
    geom_abline(intercept = i_dry, slope = s_dry,
                color = "#FD5959", linewidth = 1.0) +
    # Set gradient color
    scale_color_gradient(low = "#FD5959",
                         high = "#2E94B9") +
    expand_limits(y=c(y_min, y_max), x=c(x_min, x_max)) +
    labs(x="Vegetation Index", y="SWIR Transformed") +
    ggtitle(paste("Trapezoid Plot - ", aoi_name)) +
    # Add coeffs as text
    annotate("text", x=0.1, y=max(plot_df$STR)*0.95,
            label = coeffs_text, size = 5) +
    # Set theme
    theme_bw() +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 18))

  plot_path <- file.path(output_dir, paste0("trapezoid_", aoi_name, ".png"))
  ggsave(plot_path, width = 10, height = 7)
  message("Scatterplot of: ", num_rows_plotted,
          " pixels \n Saved to: ", plot_path)
}
