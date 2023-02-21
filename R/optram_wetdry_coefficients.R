#' @title Derive coefficients of slope and intercept
#' 
#' @description Derive slope and intercept coefficients
#' for both wet and dry trapezoid lines.
#' Write coefficients to a CSV file (as input to `optram_soilmoisture()` function)
#'
#' @param full_df, data.frame of STR and NDVI values
#' @param step, float
#'
#' @return list of float, coefficients of wet-dry trapezoid
#' @export
#' @examples print("Running optram_wetdry_coefficients.R")
#'
optram_wetdry_coefficients <- function(full_df, step=0.001){
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
  NDVI_min_max <- round(stats::quantile(full_df$NDVI, c(0.5, 0.95)) , 2)
  NDVI_series <- seq(NDVI_min_max[[1]], NDVI_min_max[[2]], step)
  message("NDVI series length:", length(NDVI_series))
  STR_NDVI_list <- lapply(NDVI_series, function(i){
    # Set NDVI value at midpoint of each interval
    ndvi_val <- i + step/2.0

    # Subset the data.frame to include only NDVI values between i and i+step
    interval_df <-  full_df[full_df$NDVI>=i & full_df$NDVI < (i+step),]
    # if too few rows in this interval, skip it, just return NULL
    if (nrow(interval_df) < 4) {
      return(NA)
    }
    # Remove lower than 10% and more than 90% quartile of STR values
    Qs <- stats::quantile(interval_df$STR, c(0.1, 0.9), na.rm=TRUE)
    interval_df <- interval_df[interval_df$STR<=Qs[[2]] &
                                 interval_df$STR>=Qs[[1]],]
    # Now, with outliers removed, find min (dry) and max (wet)
    # Within each interval
    str_max <- max(interval_df$STR, na.rm = TRUE)
    str_min <- min(interval_df$STR, na.rm = TRUE)
    str_ndvi_df1 <- data.frame("NDVI" = ndvi_val,
                               "STR_wet" = str_max,
                               "STR_dry" = str_min)
    return(str_ndvi_df1)
  })
  # Bind all interval results into one long DF
  STR_NDVI_list <- STR_NDVI_list[ !is.na(STR_NDVI_list) ]
  STR_NDVI_df <- do.call(rbind, STR_NDVI_list)
  utils::write.csv(STR_NDVI_df,
            file.path(Output_dir, "STR_NDVI_df.csv"),
            row.names = FALSE)
  # Run linear regression between STR and NDVI
  # to determine the intercept and slope for both wet and dry data
  wet_fit <- stats::lm(STR_wet ~ NDVI, data=STR_NDVI_df)
  dry_fit <- stats::lm(STR_dry ~ NDVI, data=STR_NDVI_df)
  i_wet <- wet_fit$coefficients[[1]]
  s_wet <- wet_fit$coefficients[[2]]
  i_dry <- dry_fit$coefficients[[1]]
  s_dry <- dry_fit$coefficients[[2]]
  coeffs <- data.frame("intercept_dry"=i_dry, "slope_dry"=s_dry,
                       "intercept_wet"=i_wet, "slope_wet"=s_wet)
  utils::write.csv(coeffs, coeffs_file, row.names=FALSE)

  return(coeffs)
}


#' @title Create scatter plot of STR-NDVI point cloud,
#' 
#' @description 
#' Plot STR-NDVI scatterplot to show dry and wet trapezoid lines
#'
#' @param full_df, data.frame of NDVI and STR pixel values
#' @param coeffs, list of floats, the slope and intercept
#'   of wet and dry regression lines
#'
#' @return None
#' @export
#' @import ggplot2
#' @examples
#' print("Running plot_ndvi_str_cloud.R")
#'
plot_ndvi_str_cloud <- function(full_df, coeffs){
  i_dry <- coeffs$intercept_dry
  s_dry <- coeffs$slope_dry
  i_wet <- coeffs$intercept_wet
  s_wet <- coeffs$slope_wet
  # We don't need the whole 18 M points! get a subset
  sample_idx <- sample(full_df$x, nrow(full_df)*0.2)
  plot_df <- full_df[sample_idx,]
  x_min <- min(plot_df$NDVI)*0.9
  x_max <- max(plot_df$NDVI)*1.05
  y_min <- 0.1
  y_max <- max(plot_df$STR[plot_df$NDVI>=x_min])*1.05
  ggplot(plot_df) +
    geom_point(aes(x=NDVI, y=STR), alpha = 0.05, size=1) +
    # Wet edge
    geom_abline(intercept = i_wet, slope = s_wet,
                color = "#2E94B9", size = 1.0) +
    # Dry edge
    geom_abline(intercept = i_dry, slope = s_dry,
                color = "#FD5959", size = 1.0) +
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

  ggsave(file.path(Output_dir, "trapezoid_plot.png"),
                  width=10, height=7)
}
