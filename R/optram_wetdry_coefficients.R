#' @title Derive Coefficients of Slope and Intercept
#' @description Derive slope and intercept coefficients
#' for both wet and dry trapezoid lines.
#' Write coefficients to a CSV file
#' (as input to `optram_calculate_soil_moisture()` function)
#' @param full_df, data.frame of STR and NDVI values
#' @param output_dir, string, directory to save coefficients CSV file
#' @param step, float, width of intervals along VI axis
#'  default 0.01
#' @param aoi_file, string, added to title of plot
#'  (Can be path to AOI file, then the file name is used in plot title)
#' @param trapezoid_method, string, how to prepare wet and dry trapezoid edges
#' Possible values: "linear", "exponential", "polynomial". See notes.
#' Default "linear"
#' @param save_plot, boolean, If TRUE (default) save scatterplot to output_dir
#' @return coeffs, list of float, coefficients of wet-dry trapezoid
#' @export
#' @note
#' The vegetation index column is named "VI" though it can represent
#' several vegetation indices, such as SAVI, or MSAVI.
#'
#' The `trapezoid method` parameter allows to choose one of three models
#' for creating the edge coefficients of the trapezoid.
#'  - "linear" prepares a simple OLS regression line
#'  along the wet and dry edges of the trapezoid.
#'  Four coefficients are returned: intercept and slope for both edges.
#'  - "exponential" creates an exponential curve fitted
#'  to the intercept and slope, following:
#'  Ambrosone, Mariapaola, Alessandro Matese, et al. 2020.
#'  “Retrieving Soil Moisture in Rainfed and Irrigated Fields
#'  Using Sentinel-2 Observations and a Modified OPTRAM Approach.”
#'  International Journal of Applied Earth Observation and Geoinformation
#'  https://doi.org/10.1016/j.jag.2020.102113.
#'  The same four coefficients as the linear model are returned.
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
#' coeffs <- optram_wetdry_coefficients(full_df, aoi_file)
#' print(coeffs)
#' \dontrun{
#' coeffs_poly <- optram_wetdry_coefficients(full_df, aoi_file,
#'                   trapezoid_method = "polynomial")
#' print(coeffs_poly)
#' }

optram_wetdry_coefficients <- function(
    full_df, aoi_file,
    output_dir = tempdir(),
    step = 0.01,
    trapezoid_method = c("linear", "exponential", "polynomial"),
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

  # Make sure no Inf or NA in full_df
  full_df <- full_df[is.finite(full_df$VI), ]
  VI_min_max <- round(stats::quantile(full_df$VI, c(0.1, 0.98)), 2)
  VI_series <- seq(VI_min_max[[1]], VI_min_max[[2]], step)
  message("VI series length:", length(VI_series))
  edges_list <- lapply(VI_series, function(i){
    # Set NDVI value at midpoint of each interval
    vi_val <- i + step/2.0

    # Subset the data.frame to include only NDVI values between i and i+step
    interval_df <-  full_df[full_df$VI>=i & full_df$VI < (i+step),]
    # if too few rows in this interval, skip it, just return NULL
    if (nrow(interval_df) < 4) {
      return(NA)
    }
    # Remove lower than 1% and more than 99% quantile of STR values
    # Use upper 99% quantile and lower 1% quantile as min/max values
    Qs <- stats::quantile(interval_df$STR, c(0.01, 0.99), na.rm=TRUE)
    #interval_df <- interval_df[interval_df$STR<=Qs[[2]] &
    #                             interval_df$STR>=Qs[[1]],]
    # Now, with outliers removed, find min (dry) and max (wet)
    # Within each interval
    #str_max <- max(interval_df$STR, na.rm = TRUE)
    #str_min <- min(interval_df$STR, na.rm = TRUE)
    str_max <- Qs[[2]]
    str_min <- Qs[[1]]
    edges_df1 <- data.frame("VI" = vi_val,
                              "STR_wet" = str_max,
                              "STR_dry" = str_min)
    return(edges_df1)
  })
  # Bind all interval results into one long DF
  edges_list <- edges_list[ !is.na(edges_list) ]
  edges_df <- do.call(rbind, edges_list)

  # Save STR and VI values to CSV
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  utils::write.csv(edges_df,
            file.path(output_dir, "trapezoid_edges.csv"),
            row.names = FALSE)

  tryCatch(
    expr = {trapezoid_method <- match.arg(trapezoid_method)},
    error = function(e) {
      message("Unrecognized trapezoid_method:",
              trapezoid_method)
      return(NULL)
    })

  coeffs <- switch(trapezoid_method,
        linear = linear_coefficients(edges_df, output_dir),
        exponential = exponential_coefficients(edges_df, output_dir),
        polynomial = polynomial_coefficients(edges_df, output_dir))

  if (save_plot) {
    plot_vi_str_cloud(full_df,
                     coeffs,
                     aoi_name,
                     trapezoid_method = trapezoid_method,
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
#' @param trapezoid_method, string, how to plot trapezoid line.
#'    either "linear" or "exponential", default is "linear"
#' @param edges_points, boolean, whether to add to the plot the
#'    linear regression points that were used to derive coefficients.
#'    default FALSE
#' @return None
#' @export
#' @import ggplot2
#' @examples
#' aoi_name <- "Test"
#' full_df <- readRDS(system.file("extdata", "VI_STR_data.rds",
#'         package = "rOPTRAM"))
#' coeffs <- read.csv(system.file("extdata", "coefficients.csv",
#'         package = "rOPTRAM"))
#' plot_vi_str_cloud(full_df, coeffs, aoi_name)
#' \dontrun{
#' plot_vi_str_cloud(full_df, coeffs, aoi_name,
#'                     trapezoid_method = "exponential")
#' }

plot_vi_str_cloud <- function(
    full_df,
    coeffs,
    aoi_name,
    output_dir = tempdir(),
    trapezoid_method = c("linear", "exponential", "polynomial"),
    edges_points = FALSE) {
  # Avoid "no visible binding for global variable" NOTE
  i_dry <- i_wet <- s_dry <- s_wet <- plot_df <- plot_path <- NULL
  x_min <- x_max <- y_min <- y_max <- VI_STR_df1 <- VI <- STR <- NULL
  STR_dry <- STR_wet <- edges_pts <- NULL

  # Pre-flight test
  if (ncol(coeffs) < 4) {
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

  # Prepare commom base plot
  # VI (x) axis limits
  x_min <- 0.0
  x_max <- max(plot_df$VI, na.rm = TRUE)
  # STR (y) axis limits
  y_min <- 0.1
  # Set max using median and (2 * IQR) as in outlier detection
  str_q3 <- stats::quantile(plot_df$STR, 0.9, na.rm = TRUE)
  y_max <- str_q3 + stats::IQR(plot_df$STR, na.rm = TRUE) * 3
  pl_base <- ggplot2::ggplot(plot_df) +
    geom_point(aes(x=VI, y=STR), color = "#0070000b", alpha = 0.3, size = 0.2) +
    lims(y=c(y_min, y_max), x=c(x_min, x_max)) +
    labs(x="Vegetation Index", y="SWIR Transformed") +
    # Set theme
    theme_bw() +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 18))

  # Separate plot items for each trapezoid_method
  trapezoid_method <- match.arg(trapezoid_method)
  pl <- switch(trapezoid_method,
          linear = plot_cloud_linear(pl_base,
                                     coeffs, aoi_name),
          exponential = plot_cloud_exponential(pl_base, plot_df,
                                               coeffs, aoi_name),
          # trapezoid edges are required for polynomial plot
          # plot)cloud_polynomial() will read edges df from output_dir
          polynomial = plot_cloud_polynomial(pl_base,
                                             output_dir, aoi_name))

  if (edges_points) {
    edges_pts <- utils::read.csv(file.path(output_dir, "trapezoid_edges.csv"))
    pl <- pl + geom_point(aes(x=VI, y=STR_wet),
                      color = "black", size=2, shape=2,
                      data = edges_pts) +
               geom_point(aes(x=VI, y=STR_dry),
                      color = "black", size=2, shape=6,
                      data = edges_pts)
  }
  pl
  plot_path <- file.path(output_dir,
                         paste0("trapezoid_",
                                aoi_name, "_",
                                trapezoid_method, ".png"))
  ggsave(plot_path, width = 10, height = 7)
  message("Scatterplot of: ", num_rows_plotted,
          " pixels \n Saved to: ", plot_path)
}
