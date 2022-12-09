#' Create scatter plot of STR-NDVI point cloud,
#' with dry and wet trapezoid lines
#'
#' @param full_df
#' @param coeffs
#'
#' @return
#' @export
#'
#' @examples
#'
Plot_STR_NDVI_Cloud <- function(full_df, coeffs){
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

