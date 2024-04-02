#' @title Derive Coefficients of Slope and Intercept
#' @description Derive slope and intercept coefficients
#' for both wet and dry trapezoid lines.
#' Write coefficients to a CSV file
#' (as input to `optram_calculate_soil_moisture()` function)
#' @param full_df, data.frame of STR and NDVI values
#' @param output_dir, string, directory to save coefficients CSV file
#' @param aoi_file, string, added to title of plot
#'  (Can be path to AOI file, then the file name is used in plot title)
#' @param save_plot, boolean, If TRUE (default) save scatterplot to output_dir
#'    If `save_plot` is TRUE, and `edge_points` is TRUE
#'    add the original regression points that were used to derive coefficients.
#'    default FALSE
#' @return rmse_df, data.frame,  RMSE values of fitted trapezoid edges
#' @export
#' @note
#' The vegetation index column is named "VI" though it can represent
#' several vegetation indices, such as SAVI, or MSAVI.
#'
#' The `trapezoid method` option (see `optram_options()`) allows to choose one of three models
#' for creating the edge coefficients of the trapezoid.
#'  - "linear" prepares a simple OLS regression line
#'  along the wet and dry edges of the trapezoid.
#'  Four coefficients are returned: intercept and slope for both edges.
#'
#'  - "exponential" creates an exponential curve fitted
#'  to the intercept and slope, following:
#'  Ambrosone, Mariapaola, Alessandro Matese, et al. 2020.
#'  “Retrieving Soil Moisture in Rainfed and Irrigated Fields
#'  Using Sentinel-2 Observations and a Modified OPTRAM Approach.”
#'  International Journal of Applied Earth Observation and Geoinformation
#'  https://doi.org/10.1016/j.jag.2020.102113.
#'
#'  - "polynomial" fits a second order polynomial curve to the
#'  wet and dry edges of the trapezoid, following:
#'  Ma, Chunfeng, Kasper Johansen, and Matthew F. McCabe. 2022.
#'  “Combining Sentinel-2 Data with an Optical-Trapezoid Approach to Infer within-Field Soil Moisture Variability
#'  and Monitor Agricultural Production Stages.”
#'  Agricultural Water Management 274 (December): 107942.
#'  https://doi.org/10.1016/j.agwat.2022.107942.
#'  This curve fitting function returns six coefficients:
#'  alpha, beta_1, and beta_2 for both wet and dry edges

#' @examples
#' aoi_file <- "Test"
#' full_df <- readRDS(system.file("extdata", "VI_STR_data.rds",
#'   package = "rOPTRAM"))
#' rmse_df <- optram_wetdry_coefficients(full_df, aoi_file)
#' print(rmse_df)
#' optram_options("trapezoid_method", "polynomial")
#' rmse_df <- optram_wetdry_coefficients(full_df, aoi_file)
#' print(rmse_df)
#'

optram_wetdry_coefficients <- function(
    full_df, aoi_file,
    output_dir = tempdir(),
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
  Qs <- str_max <- str_min <- interval_df <- edges_df1 <- NULL

  #  Pre-flight Check
  if (is.null(aoi_file)) {
    aoi_name <- NULL
    } else if (!file.exists(aoi_file)) {
      aoi_name <- aoi_file
      } else {aoi_name <- aoi_to_name(aoi_file)}

  trapezoid_method <- getOption("optram.trapezoid_method")
  vi_step <- getOption("optram.vi_step")
  # Make sure no Inf or NA in full_df
  full_df <- full_df[is.finite(full_df$VI), ]
  VI_min_max <- round(stats::quantile(full_df$VI, c(0.02, 0.99)), 2)
  VI_series <- seq(VI_min_max[[1]], VI_min_max[[2]], vi_step)
  message("VI series length:", length(VI_series))

  get_edges <- function(VI_series, full_df) {
	  # Internal function to extract edges points
	  # along both wet and dry trapezoid edges
    # Returns data frame of edge points with VI and STR values
	  edges_list <- lapply(VI_series, function(i){
  		# Set NDVI value at midpoint of each interval
  		vi_val <- i + vi_step/2.0
  		# Subset the data.frame to include only NDVI values between i and i+vi_step
  		interval_df <-  full_df[full_df$VI>=i & full_df$VI < (i+vi_step),]
  		# if too few data points in this interval, skip it, just return NULL
  		# Reasoning for cutoff of 20:
  		# a minimum area of 100x100 m (1 Ha) is covered by 100 Sentinel pixels.
  		# With a minimum time series of 20 images, there will be 2000 data points
  		# With vi_step = 0.02, then ~ 50 steps,
  		# so most intervals will have > 40 data points.
  		if (nrow(interval_df) < 20) {
  		  return(NA)
  		}
  		# Remove outliers, using IQR*1.5 rule
  		quartiles <- stats::quantile(interval_df$STR, c(0.25, 0.75))
  		iqr <- stats::IQR(interval_df$STR) / 1.349
  		upper_bound <- quartiles[[2]] + iqr*1.5
  		lower_bound <- quartiles[[1]] - iqr*1.5
  		interval_df <- interval_df[interval_df$STR < upper_bound &
  								   interval_df$STR > lower_bound,]
  		# Now find the lower 5% and upper 95% quantile of STR values
  		# Use these values as min/max values for dry/wet edge points
  		Qs <- stats::quantile(interval_df$STR, c(0.05, 0.95), na.rm=TRUE)
  		str_max <- Qs[[2]]
  		str_min <- Qs[[1]]
  		edges_df1 <- data.frame("VI" = vi_val,
  								"STR_wet" = str_max,
  								"STR_dry" = str_min)
  		return(edges_df1)
	  })
  	# Bind all interval results into one DF
	edges_list <- edges_list[ !is.na(edges_list) ]
	edges_df <- do.call(rbind, edges_list)
	return(edges_df)
  }
  edges_df <- get_edges(VI_series, full_df)
  if (nrow(edges_df) < 0.5 * length(VI_series)) {
    message("Too many edge points were dropped!\n",
            "Consider choosing a larger vi_step parameter.")
    return(NULL)
  }

  # Save STR and VI values to CSV
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  utils::write.csv(edges_df,
            file.path(output_dir, "trapezoid_edges.csv"),
            row.names = FALSE)

  tryCatch(
    expr = {trapezoid_method <- match.arg(trapezoid_method)},
    error = function(e) { return(NULL) })

  # Get fitted trapezoid curves
  fitted_df <- switch(trapezoid_method,
        linear = linear_coefficients(edges_df, output_dir),
        exponential = exponential_coefficients(edges_df, output_dir),
        polynomial = polynomial_coefficients(edges_df, output_dir))

  if (save_plot) {
    plot_vi_str_cloud(full_df,
                     aoi_name,
                     fitted_df,
                     output_dir = output_dir)
  }
  rmse_wet <- sqrt(mean((fitted_df$STR_wet_fit - fitted_df$STR_wet)^2))
  rmse_dry <- sqrt(mean((fitted_df$STR_dry_fit - fitted_df$STR_dry)^2))
  return(data.frame("RMSE wet" = rmse_wet, "RMSE dry" = rmse_dry))
}


#' @title Create Scatter Plot of STR-NDVI Data Points,
#' @description
#' Plot STR-NDVI scatterplot to show dry and wet trapezoid lines
#' over scatterplot of multi-temporal STR and NDVI pixel values
#' @param full_df, data.frame of NDVI and STR pixel values
#' @param aoi_name, string, used in plot title
#' @param edges_df, data.frame, points along the wet/dry edges for trapezoid
#' @param output_dir, string, directory to save plot png file.
#' @return None
#' @export
#' @import ggplot2
#' @examples
#' aoi_name <- "Test"
#' full_df <- readRDS(system.file("extdata", "VI_STR_data.rds",
#'         package = "rOPTRAM"))
#' edges_df <- read.csv(system.file("extdata", "trapezoid_edges_lin.csv",
#'                         package = "rOPTRAM"))
#' plot_vi_str_cloud(full_df, aoi_name, edges_df)
#' edges_df <- read.csv(system.file("extdata", "trapezoid_edges_poly.csv",
#'                         package = "rOPTRAM"))
#' optram_options("trapezoid_method", "polynomial")
#' plot_vi_str_cloud(full_df, aoi_name, edges_df)
#'
plot_vi_str_cloud <- function(
    full_df,
    aoi_name,
    edges_df,
    output_dir = tempdir()) {
  # Avoid "no visible binding for global variable" NOTE
  VI <- STR <- STR_dry_fit <- STR_wet_fit <- STR_wet <- NULL
  Density <- STR_dry <- NULL
  # Pre-flight test
  if (! "STR" %in% names(full_df)) {
    message("STR column missing from data.frame. Exiting...")
    return(NULL)
  }
  if (! "VI" %in% names(full_df)) {
    message("VI column missing from data.frame. Exiting...")
    return(NULL)
  }

  # We don't need Millions of points! get a subset
  num_rows <- nrow(full_df)
  if (num_rows < 400000) {
    plot_df <- full_df
  } else {
    # This trick drops the num of plotted points by orders of magnitude
    samp_num <- num_rows  / log(num_rows)
    sample_idx <- sample(num_rows, samp_num)
    plot_df <- full_df[sample_idx, ]
  }
  num_rows_plotted <- nrow(plot_df)

  trapezoid_method <- getOption("optram.trapezoid_method")
  edge_points <- getOption("optram.edge_points")
  plot_density <- getOption("optram.plot_density")
  # Prepare commom base plot
  # VI (x) axis limits
  x_min <- 0.0
  x_max <- max(plot_df$VI, na.rm = TRUE)
  # STR (y) axis limits
  y_min <- 0.1
  # Set max using median and (2 * IQR) as in outlier detection
  str_q3 <- stats::quantile(plot_df$STR, 0.9, na.rm = TRUE)
  y_max <- str_q3 + stats::IQR(plot_df$STR, na.rm = TRUE) * 3

  # Start plot
  if (plot_density == "colors") {
    pl <- ggplot2::ggplot(plot_df) +
      geom_point(aes(x = VI, y = STR, color = Density),
                 size = 0.2, alpha = 0.1) +
      scale_color_continuous(type = "viridis",
                             direction = -1) +
      theme(legend.position = "none")
  } else if (plot_density == "contours") {
    pl <- ggplot2::ggplot(plot_df) +
      geom_point(aes(x=VI, y=STR), color = "green",
                 alpha = 0.1, size = 0.2) +
      geom_density2d(aes(x=VI, y=STR), color = "darkgrey")
  } else { # No point density enhancement
    pl <- ggplot2::ggplot(plot_df) +
      geom_point(aes(x=VI, y=STR), color = "green",
                 alpha = 0.1, size = 0.2)
  }
  pl <-  pl + lims(y=c(y_min, y_max), x=c(x_min, x_max)) +
    labs(x=getOption("optram.veg_index"), y="SWIR Transformed") +
    # Dry edge
    geom_smooth(data = edges_df,
                mapping = aes(x = VI, y = STR_dry_fit),
                method = "loess",
                color = "orange2", se = FALSE,
                linewidth = 2) +
    # Wet edge
    geom_smooth(data = edges_df,
                aes(x = VI, y = STR_wet_fit),
                method = "loess",
                color="blue", se = FALSE,
                linewidth = 2) +

    ggtitle(paste("Trapezoid Plot - ", aoi_name),
            subtitle = paste(trapezoid_method, "fit"))
    # Set theme
    theme_bw() +
    theme(legend.position = "none",
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 18))

  if (edge_points) {
    pl <- pl + geom_point(aes(x=VI, y=STR_wet),
                      color = "black", size=1.5, shape=2,
                      data = edges_df) +
               geom_point(aes(x=VI, y=STR_dry),
                      color = "black", size=1.5, shape=6,
                      data = edges_df)
  }

  plot_path <- file.path(output_dir,
                         paste0("trapezoid_",
                                aoi_name, "_",
                                trapezoid_method, ".png"))
  ggsave(plot_path,
         plot = pl, width = 10, height = 7, units = "in", dpi = 192)
}
