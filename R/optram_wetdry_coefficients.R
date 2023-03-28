#' @title Derive coefficients of slope and intercept
#' @description Derive slope and intercept coefficients
#' for both wet and dry trapezoid lines.
#' Write coefficients to a CSV file (as input to `optram_soilmoisture()` function)
#' @param full_df, data.frame of STR and NDVI values
#' @param output_dir, string, directory to save coefficients CSV file
#' @param step, float
#' @param save_plot, boolean, If TRUE (default) save scatterplot to output_dir
#' @return coeffs, list of float, coefficients of wet-dry trapezoid
#' @export
#' @examples print("Running optram_wetdry_coefficients.R")

optram_wetdry_coefficients <- function(full_df,
                                       output_dir = tempdir(),
                                       step=0.001,
                                       save_plot = TRUE) {
  # Derive slope and intercept to two sides of trapezoid
  # Based on:
  # https://github.com/teerathrai/OPTRAM
  #
  # Parameters:
  #   full_df: data.frame, table of NDVIU and STR values
  # Returns:
  #   coeffs: array of floats, slope & intercept for both wet and dry lines

  # Create series of values for NDVI
  # Get min/max values from NDVI data, slightly smaller than full range

  # Avoid "no visible binding for global variable" NOTE
  VI_min_max <- VI_series <- VI_STR_list <- VI_STR_df <- NULL
  Qs <- str_max <- str_min <- interval_df <- VI_STR_df1 <- NULL
  
  VI_min_max <- round(stats::quantile(full_df$NDVI, c(0.2, 0.98)) , 2)
  VI_series <- seq(VI_min_max[[1]], VI_min_max[[2]], step)
  message("NDVI series length:", length(VI_series))
  VI_STR_list <- lapply(VI_series, function(i){
    # Set NDVI value at midpoint of each interval
    ndvi_val <- i + step/2.0

    # Subset the data.frame to include only NDVI values between i and i+step
    interval_df <-  full_df[full_df$NDVI>=i & full_df$NDVI < (i+step),]
    # if too few rows in this interval, skip it, just return NULL
    if (nrow(interval_df) < 4) {
      return(NA)
    }
    # Remove lower than 10% and more than 90% quartile of STR values
    Qs <- stats::quantile(interval_df$STR, c(0.02, 0.98), na.rm=TRUE)
    interval_df <- interval_df[interval_df$STR<=Qs[[2]] &
                                 interval_df$STR>=Qs[[1]],]
    # Now, with outliers removed, find min (dry) and max (wet)
    # Within each interval
    str_max <- max(interval_df$STR, na.rm = TRUE)
    str_min <- min(interval_df$STR, na.rm = TRUE)
    VI_STR_df1 <- data.frame("NDVI" = ndvi_val,
                               "STR_wet" = str_max,
                               "STR_dry" = str_min)
    return(VI_STR_df1)
  })
  # Bind all interval results into one long DF
  VI_STR_list <- VI_STR_list[ !is.na(VI_STR_list) ]
  VI_STR_df <- do.call(rbind, VI_STR_list)
  utils::write.csv(VI_STR_df,
            file.path(output_dir, "VI_STR_df.csv"),
            row.names = FALSE)
  # Run linear regression between STR and NDVI
  # to determine the intercept and slope for both wet and dry data
  wet_fit <- stats::lm(STR_wet ~ NDVI, data=VI_STR_df)
  dry_fit <- stats::lm(STR_dry ~ NDVI, data=VI_STR_df)
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
                                output_dir = output_dir)
  }
  return(coeffs)
}


#' @title Create scatter plot of STR-NDVI point cloud,
#' @description
#' Plot STR-NDVI scatterplot to show dry and wet trapezoid lines
#' over scatterplot of multi-temporal STR and NDVI pixel values
#' @param full_df, data.frame of NDVI and STR pixel values
#' @param coeffs, list of floats, the slope and intercept
#'   of wet and dry regression lines
#' @param output_dir, string, directory to save plot png file.
#' @return None
#' @export
#' @import ggplot2
#' @examples
#' print("Running plot_ndvi_str_cloud.R")

plot_ndvi_str_cloud <- function(full_df,
                                coeffs,
                                output_dir = tempdir()){
  # Avoid "no visible binding for global variable" NOTE
  i_dry <- i_wet <- s_dry <- s_wet <- plot_df <- plot_path <- NULL
  x_min <- x_max <- y_min <- y_max <- VI_STR_df1 <- NULL
  NDVI <- STR <- NULL

  i_dry <- coeffs$intercept_dry
  s_dry <- coeffs$slope_dry
  i_wet <- coeffs$intercept_wet
  s_wet <- coeffs$slope_wet
  # We don't need Millions of points! get a subset
  if (length(full_df$x) > 10000){
    sample_idx <- sample(full_df$x, 10000)
    plot_df <- full_df[sample_idx,]
  } else {
    plot_df <- full_df
  }
  x_min <- min(plot_df$NDVI)*0.9
  x_max <- max(plot_df$NDVI)*1.05
  y_min <- 0.1
  y_max <- max(plot_df$STR)*1.05
  ggplot2::ggplot(plot_df) +
    geom_point(aes(x=NDVI, y=STR), alpha = 0.05, size=1) +
    # Wet edge
    geom_abline(intercept = i_wet, slope = s_wet,
                color = "#2E94B9", linewidth = 1.0) +
    # Dry edge
    geom_abline(intercept = i_dry, slope = s_dry,
                color = "#FD5959", linewidth = 1.0) +
    # Set gradient color
    scale_color_gradient(low="#FD5959",
                         high="#2E94B9") +
    expand_limits(y=c(y_min, y_max), x=c(x_min, x_max)) +
    labs(x="SAVI", y="SWIR Transformed") +
    ggtitle("Trapezoid Plot") +
    # Set theme
    theme_bw() +
    theme(axis.title = element_text(size=14),
          axis.text = element_text(size=12),
          plot.title = element_text(size=18))

  plot_path <- file.path(output_dir, "trapezoid_plot.png")
  ggsave(plot_path, width=10, height=7)
  message("Scatterplot saved to: ", plot_path)
}
