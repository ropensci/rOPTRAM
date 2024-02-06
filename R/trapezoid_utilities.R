#' @title Utility Function to prepare linear regression edges of trapezoid
#' @description Called by `optram_wetdry_coefficients()`
#' to prepare linear regression line along trapezoid edges
#' Calculates the intercept and slope of both wet and dry edges
#' Not exported
#' @param df, data.frame, values of VI and STR along edges of trapezoid
#' @param output_dir, string, path to save coefficients csv file
#' @return coeffs, data.frame of 2 linear regression coefficients
#'  for both wet and dry edges
#' @examples
#' \dontrun{
#' edges_file <- system.file("extdata/trapezoid_edges.csv",
#'                            package = "rOPTRAM")
#' df <- read.csv(edges_file)
#' output_dir <- tempdir()
#' coeffs <- linear_coefficients(df, output_dir)
#' coeffs
#' }
linear_coefficients <- function(df, output_dir) {
  wet_fit <- stats::lm(STR_wet ~ VI, data = df)
  dry_fit <- stats::lm(STR_dry ~ VI, data = df)
  i_wet <- wet_fit$coefficients[[1]]
  s_wet <- wet_fit$coefficients[[2]]
  i_dry <- dry_fit$coefficients[[1]]
  s_dry <- dry_fit$coefficients[[2]]
  coeffs <- data.frame("intercept_dry"=i_dry, "slope_dry"=s_dry,
                       "intercept_wet"=i_wet, "slope_wet"=s_wet)
  utils::write.csv(coeffs,
                   file.path(output_dir, "coefficients.csv"),
                   row.names=FALSE)
  return(coeffs)
}

#' @title Utility Function to prepare exponential fitted edges of trapezoid
#' @description Called by `optram_wetdry_coefficients()`
#' to prepare exponential curve along trapezoid edges
#' Calculates the intercept and slope of both wet and dry edges
#' and updates the edges data.frame with these exp fitted values
#' Not exported
#' @param df, data.frame, values of VI and STR along edges of trapezoid
#' @param output_dir, string, path to save coefficients csv file
#' @return coeffs, data.frame of 2 linear regression coefficients
#'  for both wet and dry edges
#' @examples
#' \dontrun{
#'   df <- read.csv(system.file("extdata", "trapezoid_edges.csv",
#'                 package = "rOPTRAM"))
#'   output_dir <- tempdir()
#'   coeffs <- rOPTRAM::exponential_coefficients(df, output_dir)
#'   coeffs
#' }
exponential_coefficients <- function(df, output_dir) {
  # Uses the same linear regression line,
  # but adds exponential curve
  wet_fit <- stats::lm(STR_wet ~ VI, data = df)
  dry_fit <- stats::lm(STR_dry ~ VI, data = df)
  i_wet <- wet_fit$coefficients[[1]]
  s_wet <- wet_fit$coefficients[[2]]
  i_dry <- dry_fit$coefficients[[1]]
  s_dry <- dry_fit$coefficients[[2]]
  coeffs <- data.frame("intercept_dry"=i_dry, "slope_dry"=s_dry,
                       "intercept_wet"=i_wet, "slope_wet"=s_wet)

  # Update the data.frame of trapezoid edges and save
  df$STR_poly_wet <- i_wet * exp(s_wet * df$VI)
  df$STR_poly_dry <- i_dry * exp(s_dry * df$VI)
  utils::write.csv(df,
                   file.path(output_dir, "trapezoid_edges_exp.csv"),
                   row.names = FALSE)
  utils::write.csv(coeffs,
                   file.path(output_dir, "coefficients_exp.csv"),
                   row.names=FALSE)
  return(coeffs)
}

#' @title Utility Function to prepare polynomial fitted edges of trapezoid
#' @description Called by `optram_wetdry_coefficients()`
#' to prepare second order polynomial curve along trapezoid edges
#' Calculates six coefficients:  intercept (alpha)  of both wet and dry edges
#' and first and second order coefficients (beta), as in
#' $STR = alpha + beta_1 * VI + beta_2 * VI^2$
#' and updates the edges data.frame with these polynomila values fitted values
#' @param df, data.frame, values of VI and STR along edges of trapezoid
#' @param output_dir, string, path to save coefficients csv file
#' @return coeffs, data.frame of 3 coefficients
#'  for both wet and dry edges
#' @examples
#' \dontrun{
#'   df <- read.csv(system.file("extdata", "trapezoid_edges.csv",
#'                   package = "rOPTRAM"))
#'   output_dir <- tempdir()
#'   coeffs <- rOPTRAM::polynomial_coefficients(df, output_dir)
#'   coeffs
#'  }
polynomial_coefficients <- function(df, output_dir) {
  wet_fit <- stats::lm(df$STR_wet ~ poly(df$VI, 2))
  dry_fit <- stats::lm(df$STR_dry ~ poly(df$VI, 2))

  coeffs <- data.frame("alpha_dry" = dry_fit$coefficients[1],
                       "beta1_dry" = dry_fit$coefficients[2],
                       "beta2_dry" = dry_fit$coefficients[3],
                       "alpha_wet" = wet_fit$coefficients[1],
                       "beta1_wet" = wet_fit$coefficients[2],
                       "beta2_wet" = wet_fit$coefficients[3])

  # Update data.frame of edges and save to trapezoid_edges.csv file
  wet_dist <- wet_fit$fitted.values
  df$STR_poly_wet <- wet_dist
  dry_dist <- dry_fit$fitted.values
  df$STR_poly_dry <- dry_dist
  utils::write.csv(df,
                   file.path(output_dir, "trapezoid_edges_poly.csv"),
                   row.names = FALSE)
  utils::write.csv(coeffs,
                   file.path(output_dir, "coefficients_poly.csv"),
                   row.names=FALSE)
  return(coeffs)
}


#' @title Calculate Soil Moisture Grid, Linear Trapezoid
#' @description Prepare soil moisture grid from STR and VI images
#'              for a single date, using linear trapezoid method, and
#'              using the derived slope and intercept coefficients
#' @param VI, terra rast, the vegetation index raster
#' @param STR, terra rast, the STR raster
#' @param coeffs, list, 4 trapezoid coefficients
#' @return rast, soil moisture grid
#' @note
#' This function is used after preparing the OPTRAM model coefficients with:
#'  `optram_wetdry_coefficients()`. Typically a new image date,
#'   (that was not used for preparing the model),
#'   will be referenced in the `img_date` parameter.
#'   The resulting soil moisture raster is saved to `output_dir`.
linear_soil_moisture <- function(coeffs, VI, STR) {
  if (ncol(coeffs) < 4) {
    message("Incorrect coefficients file. Exiting...")
    return(NULL)
  }
  i_dry <- coeffs$intercept_dry
  s_dry <- coeffs$slope_dry
  i_wet <- coeffs$intercept_wet
  s_wet <- coeffs$slope_wet
  W <- (i_dry + s_dry*VI - STR) / (i_dry - i_wet +  (s_dry-s_wet)*VI)
  return(W)
}

#' @title Calculate Soil Moisture Grid, Exponential Trapezoid
#' @description Prepare soil moisture grid from STR and VI images
#'              for a single date, based on exponential function
#'              to derive trapezoid edges,
#'              using the derived slope and intercept coefficients
#' @param VI, terra rast, the vegetation index raster
#' @param STR, terra rast, the STR raster
#' @param coeffs, list, 4 trapezoid coefficients
#' @return rast, soil moisture grid
#' @note
#' This function is used after preparing the OPTRAM model coefficients with:
#'  `optram_wetdry_coefficients()`. Typically a new image date,
#'   (that was not used for preparing the model),
#'   will be referenced in the `img_date` parameter.
#'   The resulting soil moisture raster is saved to `output_dir`.
#' This function implements an exponential trapezoid, following:
#' Ambrosone, Mariapaola, et al. 2020.
#'  “Retrieving Soil Moisture in Rainfed and Irrigated Fields
#'   Using Sentinel-2 Observations and a Modified OPTRAM Approach.”
#'  International Journal of Applied Earth Observation and Geoinformation 89 (July):
#'   102113. https://doi.org/10.1016/j.jag.2020.102113.
#' @examples
#' img_date <- "2023-03-11"
#' VI_dir <- system.file("extdata", "SAVI", package = "rOPTRAM")
#' STR_dir <- system.file("extdata", "STR", package = "rOPTRAM")
#' coeffs_file <- system.file("extdata", "coefficients.csv",
#'         package = "rOPTRAM")
#' SM <- optram_calculate_soil_moisture(img_date, VI_dir, STR_dir, coeffs_file,
#'                                      trapezoid_method = "exponential")
exponential_soil_moisture <- function(coeffs, VI, STR) {
  # based on STR and two STR_dry and STR_wet bands
  # Using the exponential trapezoid edges
  # Following:
  #  Ambrosone, Mariapaola, Alessandro Matese, Salvatore Filippo Di Gennaro, Beniamino Gioli,
  #  Marin Tudoroiu, Lorenzo Genesio, Franco Miglietta, et al. 2020.
  #  “Retrieving Soil Moisture in Rainfed and Irrigated Fields Using Sentinel-2 Observations
  #   and a Modified OPTRAM Approach.”
  #  International Journal of Applied Earth Observation and Geoinformation 89 (July):
  #   102113. https://doi.org/10.1016/j.jag.2020.102113.
  #
  # W = (i_dry * exp(s_dry * VI) - STR) / (i_dry * exp(s_dry*VI) - i_wet * exp(s_wet * VI))
  if (ncol(coeffs) < 4) {
    message("Incorrect coefficients file. Exiting...")
    return(NULL)
  }
  i_dry <- coeffs$intercept_dry
  s_dry <- coeffs$slope_dry
  i_wet <- coeffs$intercept_wet
  s_wet <- coeffs$slope_wet
  W <- (i_dry * exp(s_dry * VI) - STR) /
    (i_dry * exp(s_dry * VI ) - i_wet * exp(s_wet * VI))

  return(W)
}


#' @title Calculate Soil Moisture, Polynomial Fitted Curve
#' @description Prepare soil moisture grid from STR and VI images
#'    for a single date, based on polynomial function fitted to trapezoid edges.
#' @param VI, terra rast, the vegetation index raster
#' @param STR, terra rast, the STR raster
#' @param coeffs, list, 6 trapezoid coefficients
#' @return rast, soil moisture grid
#' @note
#' This function is used after preparing the OPTRAM model coefficients with:
#'  `optram_wetdry_coefficients()`. Typically a new image date,
#'   (that was not used for preparing the model),
#'   will be referenced in the `img_date` parameter.
#'   The resulting soil moisture raster is saved to `output_dir`.
#' This function implements an polynomial fitted curve, following:
#' Ma, Chunfeng, Kasper Johansen, and Matthew F. McCabe. 2022.
#'  “Combining Sentinel-2 Data with an Optical-Trapezoid Approach to Infer within-Field Soil Moisture Variability
#'  and Monitor Agricultural Production Stages.”
#'  Agricultural Water Management 274 (December): 107942.
#'  https://doi.org/10.1016/j.agwat.2022.107942.
#' @examples
#' img_date <- "2023-03-11"
#' VI_dir <- system.file("extdata", "SAVI", package = "rOPTRAM")
#' STR_dir <- system.file("extdata", "STR", package = "rOPTRAM")
#' coeffs_file <- system.file("extdata", "coefficients.csv",
#'         package = "rOPTRAM")
#' SM <- optram_calculate_soil_moisture(img_date, VI_dir, STR_dir, coeffs_file,
#'                                      trapezoid_method = "polynomial")
#'
polynomial_soil_moisture <- function(coeffs, VI, STR) {
  # based on STR and two STR_dry and STR_wet bands
  # Using the exponential trapezoid edges
  # Following:
  #  Ambrosone, Mariapaola, Alessandro Matese, Salvatore Filippo Di Gennaro, Beniamino Gioli,
  #  Marin Tudoroiu, Lorenzo Genesio, Franco Miglietta, et al. 2020.
  #  “Retrieving Soil Moisture in Rainfed and Irrigated Fields Using Sentinel-2 Observations
  #   and a Modified OPTRAM Approach.”
  #  International Journal of Applied Earth Observation and Geoinformation 89 (July):
  #   102113. https://doi.org/10.1016/j.jag.2020.102113.
  #
  # W = (STR − (alpha_dry + beta1_dry * VI + beta2_dry * VI^2)) /
  #     ((alpha-wet - alpha_dry) + (beta1_wet - beta1_dry)*VI + (beta2_wet - beta2_dry)*VI^2)

  if (ncol(coeffs) < 6) {
    message("Incorrect coefficients file. Exiting...")
    return(NULL)
  }
  a_wet <- coeffs$alpha_wet
  b1_wet <- coeffs$beta1_wet
  b2_wet <- coeffs$beta2_wet
  a_dry <- coeffs$alpha_dry
  b1_dry <- coeffs$beta1_dry
  b2_dry <- coeffs$beta2_dry

  W <- (STR - (a_dry + b1_dry * VI + b2_dry * VI^2)) /
       ((a_wet - a_dry) + (b1_wet - b1_dry)*VI + (b2_wet - b2_dry)*VI^2)

  return(W)
}



#' @title Scatterplot, Linear Fitted Curve
#' @description Plot of VI/STR points, with linear fitted curve
#' @param pl_base, ggplot object, additional lines are added to base plot
#' @param coeffs, data.frame, 4 trapezoid coefficients
#' @param aoi_name, string, added to plot title
#' @return ggplot object
#' @note
#' This function is called from `plot_vi_str_cloud()`
#' @examples
#' \dontrun{
#' plot_df <- readRDS(system.file("extdata",
#'                                "VI_STR_data.RDS",package = "rOPTRAM")
#' coeffs_file <- system.file("extdata", "coefficients.csv",
#'         package = "rOPTRAM")
#' coeffs <- read.csv(coeffs_file)
#' pl <- plot_cloud_linear(plot_df, coeffs)
#' }
plot_cloud_linear <- function(pl_base, coeffs, aoi_name) {
  i_dry <- round(coeffs$intercept_dry, 3)
  s_dry <- round(coeffs$slope_dry, 3)
  i_wet <- round(coeffs$intercept_wet, 3)
  s_wet <- round(coeffs$slope_wet, 3)
  # Text to add to plot
  coeffs_text <- paste("Dry intercept:", i_dry, "\n Dry slope:", s_dry,
                       "\n Wet intercept:", i_wet, "\n Wet slope:", s_wet)
  pl <- pl_base +
    # Wet edge
    geom_abline(intercept = i_wet, slope = s_wet,
                color = "#2E94B9", linewidth = 1.0) +
    # Dry edge
    geom_abline(intercept = i_dry, slope = s_dry,
                color = "#FD5959", linewidth = 1.0) +

    ggtitle(paste("Trapezoid Plot - ", aoi_name), subtitle = "Linear fit") +
    # Add coeffs as text
    annotate("text", x=0.1, y=mean(pl_base$data$STR, na.rm = TRUE),
             label = coeffs_text, size = 5)

  return(pl)
}


#' @title Scatterplot, Exponential Fitted Curve
#' @description Plot of VI/STR points, with exponential fitted curve
#' @param pl_base, ggplot object, additional lines are added to base plot
#' @param plot_df, data.frame, original plot data, needed for Exponential fitting
#' @param coeffs, data.frame, 4 trapezoid coefficients
#' @param aoi_name, string, added to plot title
#' @return ggplot object
#' @note
#' This function is called from `plot_vi_str_cloud()`
#' @examples
#' \dontrun{
#' plot_df <- readRDS(system.file("extdata",
#'                                "VI_STR_data.RDS",package = "rOPTRAM")
#' coeffs_file <- system.file("extdata", "coefficients.csv",
#'         package = "rOPTRAM")
#' coeffs <- read.csv(coeffs_file)
#' pl <- plot_cloud_exponential(plot_df, coeffs)
#' }
plot_cloud_exponential <- function(pl_base, plot_df, coeffs, aoi_name) {
  i_dry <- round(coeffs$intercept_dry, 3)
  s_dry <- round(coeffs$slope_dry, 3)
  i_wet <- round(coeffs$intercept_wet, 3)
  s_wet <- round(coeffs$slope_wet, 3)

  # Add exponential function lines to the graphs
  str_wet  <- function(VI = plot_df$VI) {
    return(i_wet * exp(s_wet * VI))
  }
  str_dry  <- function(VI = plot_df$VI) {
    return(i_dry * exp(s_dry * VI))
  }
  pl <- pl_base +
    geom_function(color = "#10607e", linewidth = 1.5, linetype = "dotted",
                  fun = str_wet) +
    geom_function(color = "#8b412a62", linewidth = 1.5, linetype = "dotted",
                  fun = str_dry) +
    ggtitle(paste("Trapezoid Plot - ", aoi_name), subtitle = "Exponential fit") +

  return(pl)
}


#' @title Scatterplot, Exponential Fitted Curve
#' @description Plot of VI/STR points, with exponential fitted curve
#' @param pl_base, ggplot object, additional lines are added to base plot
#' @param output_dir, string, path to previously saved trapezoid_edges_poly.csv
#' @param aoi_name, string, added to plot title
#' @return ggplot object
#' @note
#' This function is called from `plot_vi_str_cloud()`
#' @examples
#' \dontrun{
#' output_dir <- system.file("extdata")
#' pl <- plot_cloud_polynomial(pl_base, output_dir)
#'}
plot_cloud_polynomial <- function(pl_base, output_dir, aoi_name) {
  # a_wet <- coeffs$alpha_wet
  # a_dry <- coeffs$alpha_dry
  # b1_wet <- coeffsbeta1_wet
  # b1_dry <- coeffs$beta1_dry
  # b2_wet <- coeffs$beta2_wet
  # b2_dry <- coeffs$beta2_dry

  # Add exponential edges points as smoothed lines to the graph
  edges <- utils::read.csv(file.path(output_dir,
                                    "trapezoid_edges_poly.csv"))
  STR_poly_wet  <- edges$STR_poly_wet
  STR_poly_dry  <- edges$STR_poly_dry

  pl <- pl_base +
    geom_smooth(data = edges,
                mapping = aes(x = VI, y = STR_poly_dry),
                method = "loess",
                color = "brown", se = FALSE) +
    geom_smooth(data = edges,
                aes(x = VI, y = STR_poly_wet),
                method = "loess",
                color="purple", se = FALSE) +
    ggtitle(paste("Trapezoid Plot - ", aoi_name), subtitle = "Polynomial fit") +

  return(pl)
}