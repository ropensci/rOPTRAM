#' @title Utility Function to Prepare Linear Regression Edges of Trapezoid
#' @description Called by \code{\link[rOPTRAM]{optram_wetdry_coefficients}}
#' to prepare linear regression line along trapezoid edges
#' Calculates the intercept and slope of both wet and dry edges
#' Not exported
#' @param df, data.frame, values of VI and STR along edges of trapezoid
#' @param output_dir, string, path to save coefficients CSV file
#' @return df, data.frame, the trapezoid line edge points
#'  with fitted wet/dry values added
#' @note
#'  Three CSV files are saved:
#'  \itemize{
#'   \item the regressions coefficients,
#'   \item the trapezoid edge points
#'   \item RMSE of the fitted curve
#'   }
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
  # Force intercept at zero
  wet_fit <- stats::lm(STR_wet ~ VI, data = df)
  dry_fit <- stats::lm(STR_dry ~ VI, data = df)
  #i_wet <- i_dry <- 0
  i_wet <- wet_fit$coefficients['(Intercept)']
  s_wet <- wet_fit$coefficients['VI']
  i_dry <- dry_fit$coefficients['(Intercept)']
  s_dry <- dry_fit$coefficients['VI']
  coeffs <- data.frame("intercept_dry"=i_dry, "slope_dry"=s_dry,
                       "intercept_wet"=i_wet, "slope_wet"=s_wet)
  # Update data.frame with fitted values
  df$STR_wet_fit <- wet_fit$fitted.values
  df$STR_dry_fit <- dry_fit$fitted.values
  rmse_wet <- sqrt(mean(wet_fit$residuals^2))
  rmse_dry <- sqrt(mean(dry_fit$residuals^2))
  rmse_df <- data.frame("RMSE dry" = rmse_dry, "RMSE wet" = rmse_wet)
  utils::write.csv(rmse_df,
                   file.path(output_dir, "trapezoid_rmse_lin.csv"),
                   row.names = FALSE)
  utils::write.csv(df,
                   file.path(output_dir, "trapezoid_edges_lin.csv"),
                   row.names = FALSE)
  utils::write.csv(coeffs,
                   file.path(output_dir, "coefficients_lin.csv"),
                   row.names=FALSE)
  return(df)
}

#' @title Utility Function to Prepare Exponential Fitted Edges of Trapezoid
#' @description Called by \code{\link[rOPTRAM]{optram_wetdry_coefficients}}
#' to prepare exponential curve along trapezoid edges.
#' Calculates the intercept and slope of both wet and dry edges
#' and updates the edges data.frame with these exp fitted values.
#' Not exported.
#'
#' @param df, data.frame, values of VI and STR along edges of trapezoid
#' @param output_dir, string, path to save coefficients CSV file
#' @return df, data.frame, the trapezoid line edge points
#'  with fitted wet/dry values added
#' @note
#' Three CSV files are saved:
#'  \itemize{
#'   \item the regressions coefficients,
#'   \item the trapezoid edge points
#'   \item RMSE of the fitted curve
#'   }
#' @examples
#' \dontrun{
#'   df <- read.csv(system.file("extdata", "trapezoid_edges.csv",
#'                 package = "rOPTRAM"))
#'   output_dir <- tempdir()
#'   coeffs <- rOPTRAM::exponential_coefficients(df, output_dir)
#'   coeffs
#' }
exponential_coefficients <- function(df, output_dir) {
  wet_fit <- stats::lm(log(df$STR_wet) ~ df$VI)
  dry_fit <- stats::lm(log(df$STR_dry) ~ df$VI)

  i_wet <- wet_fit$coefficients[[1]]
  s_wet <- wet_fit$coefficients[[2]]
  i_dry <- dry_fit$coefficients[[1]]
  s_dry <- dry_fit$coefficients[[2]]
  coeffs <- data.frame("intercept_dry"=i_dry, "slope_dry"=s_dry,
                       "intercept_wet"=i_wet, "slope_wet"=s_wet)

  # Update the data.frame of trapezoid edges and save
  df$STR_wet_fit <- exp(wet_fit$fitted.values)
  df$STR_dry_fit <- exp(dry_fit$fitted.values)
  rmse_wet <- sqrt(mean(wet_fit$residuals^2))
  rmse_dry <- sqrt(mean(dry_fit$residuals^2))
  rmse_df <- data.frame("RMSE dry" = rmse_dry, "RMSE wet" = rmse_wet)
  utils::write.csv(rmse_df,
                   file.path(output_dir, "trapezoid_rmse_exp.csv"),
                   row.names = FALSE)

  utils::write.csv(df,
                   file.path(output_dir, "trapezoid_edges_exp.csv"),
                   row.names = FALSE)
  utils::write.csv(coeffs,
                   file.path(output_dir, "coefficients_exp.csv"),
                   row.names=FALSE)
  return(df)
}


#' @title Utility Function to Prepare Polynomial Fitted Edges of Trapezoid
#' @description Called by \code{\link[rOPTRAM]{optram_wetdry_coefficients}} to prepare second order polynomial curve along trapezoid edges
#' Calculates six coefficients:  intercept (alpha)  of both wet and dry edges
#' and first and second order coefficients (beta), as in
#' \eqn{STR = alpha + beta_1 * VI + beta_2 * VI^2}
#' and updates the edges data.frame with these polynomila values fitted values
#' @param df, data.frame, values of VI and STR along edges of trapezoid
#' @param output_dir, string, path to save coefficients CSV file
#' @return df, data.frame, the trapezoid line edge points with fitted values added
#'  for both wet and dry edges
#' @note
#'  Three CSV files are saved:
#' \itemize{
#'   \item the regressions coefficients,
#'   \item the trapezoid edge points
#'   \item RMSE of the fitted curve
#' }
#' @examples
#' \dontrun{
#'   df <- read.csv(system.file("extdata", "trapezoid_edges.csv",
#'                   package = "rOPTRAM"))
#'   output_dir <- tempdir()
#'   coeffs <- rOPTRAM::polynomial_coefficients(df, output_dir)
#'   coeffs
#'  }
#'
polynomial_coefficients <- function(df, output_dir) {
  wet_fit <- stats::lm(STR_wet ~ poly(VI, degree = 2, raw = TRUE),
                      data = df)
  dry_fit <- stats::lm(STR_dry ~ poly(VI, degree = 2, raw = TRUE),
                      data = df)

  coeffs <- data.frame("alpha_dry" = dry_fit$coefficients[1],
                       "beta1_dry" = dry_fit$coefficients[2],
                       "beta2_dry" = dry_fit$coefficients[3],
                       "alpha_wet" = wet_fit$coefficients[1],
                       "beta1_wet" = wet_fit$coefficients[2],
                       "beta2_wet" = wet_fit$coefficients[3])

  # Update data.frame of edges and save to new edges file
  df$STR_wet_fit <- wet_fit$fitted.values
  df$STR_dry_fit <- dry_fit$fitted.values
  rmse_wet <- sqrt(mean(wet_fit$residuals^2))
  rmse_dry <- sqrt(mean(dry_fit$residuals^2))
  rmse_df <- data.frame("RMSE dry" = rmse_dry, "RMSE wet" = rmse_wet)
  utils::write.csv(rmse_df,
                   file.path(output_dir, "trapezoid_rmse_poly.csv"),
                   row.names = FALSE)
  utils::write.csv(df,
                   file.path(output_dir, "trapezoid_edges_poly.csv"),
                   row.names = FALSE)
  utils::write.csv(coeffs,
                   file.path(output_dir, "coefficients_poly.csv"),
                   row.names=FALSE)
  return(df)
}


#' @title Calculate Soil Moisture Grid, Linear Trapezoid
#' @description Prepare soil moisture grid from STR and VI images
#'              for a single date, using linear trapezoid method, and
#'              using the derived slope and intercept coefficients
#' @param VI, SpatRaster, the vegetation index raster
#' @param STR, SpatRaster, the STR raster
#' @param coeffs, list, 4 trapezoid coefficients
#' @return rast, soil moisture grid
#' @examples
#' img_date <- "2023-03-11"
#' VI_dir <- system.file("extdata", "NDVI", package = "rOPTRAM")
#' STR_dir <- system.file("extdata", "STR", package = "rOPTRAM")
#' SM <- optram_calculate_soil_moisture(img_date,
#'             VI_dir, STR_dir,
#'             data_dir = tempdir())
#' @note
#' This function is used after preparing the OPTRAM model coefficients with:
#'  \code{\link[rOPTRAM]{optram_wetdry_coefficients}}. Typically a new image date,
#'   (that was not used for preparing the model),
#'   will be referenced in the \code{img_date} parameter.
#'   The resulting soil moisture raster is saved to \code{output_dir}.
linear_soil_moisture <- function(coeffs, VI, STR) {
  if (ncol(coeffs) < 4) {
    message("Incorrect number of coefficients. Exiting...")
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
#'  \code{\link[rOPTRAM]{optram_wetdry_coefficients}}. Typically a new image date, (that was not used for preparing the model),
#'   will be referenced in the \code{img_date} parameter.
#'   The resulting soil moisture raster is saved to \code{output_dir}.
#' This function implements an exponential trapezoid, following:
#' Ambrosone, Mariapaola, et al. 2020.
#'  “Retrieving Soil Moisture in Rainfed and Irrigated Fields
#'   Using Sentinel-2 Observations and a Modified OPTRAM Approach.”
#'  International Journal of Applied Earth Observation and Geoinformation 89 (July):
#'   102113. \doi{https://doi.org/10.1016/j.jag.2020.102113}
#' @examples
#' img_date <- "2023-03-11"
#' VI_dir <- system.file("extdata", "NDVI", package = "rOPTRAM")
#' STR_dir <- system.file("extdata", "STR", package = "rOPTRAM")
#' SM <- optram_calculate_soil_moisture(img_date,
#'             VI_dir, STR_dir,
#'             data_dir = tempdir())
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
  # Below uses linear fitted equation, above uses exponential
  # W_lo = (i_dry + s_dry*VI_lo - STR) /
  #        (i_dry - i_wet +  (s_dry-s_wet)*VI_lo)
  # W_hi = (i_dry * exp(s_dry * VI_hi) - STR) /
  #        (i_dry * exp(s_dry*VI_hi) - i_wet * exp(s_wet * VI_hi))
  if (ncol(coeffs) < 4) {
    message("Incorrect coefficients file. Exiting...")
    return(NULL)
  }
  i_dry <- coeffs$intercept_dry
  s_dry <- coeffs$slope_dry
  i_wet <- coeffs$intercept_wet
  s_wet <- coeffs$slope_wet

  W <- (i_dry * exp(s_dry*VI) - STR) /
    (i_dry * exp(s_dry*VI) - (i_wet * exp(s_wet*VI)))
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
#'  \code{\link[rOPTRAM]{optram_wetdry_coefficients}}. Typically a new image date,  (that was not used for preparing the model),
#'   will be referenced in the \code{img_date} parameter.
#'   The resulting soil moisture raster is saved to \code{output_dir}
#' This function implements an polynomial fitted curve, following:
#' Ma, Chunfeng, Kasper Johansen, and Matthew F. McCabe. 2022.
#'  “Combining Sentinel-2 Data with an Optical-Trapezoid Approach to Infer within-Field Soil Moisture Variability
#'  and Monitor Agricultural Production Stages.”
#'  Agricultural Water Management 274 (December): 107942.
#'  \doi{https://doi.org/10.1016/j.agwat.2022.107942}.
#' @examples
#' img_date <- "2023-03-11"
#' VI_dir <- system.file("extdata", "NDVI", package = "rOPTRAM")
#' STR_dir <- system.file("extdata", "STR", package = "rOPTRAM")
#' optram_options("trapezoid_method", "polynomial")
#' SM <- optram_calculate_soil_moisture(img_date,
#'                           VI_dir, STR_dir,
#'                           data_dir = tempdir())
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
  #   102113. \doi{https://doi.org/10.1016/j.jag.2020.102113}.
  #
  # \deqn{W = (STR − (alpha_dry + beta1_dry * VI + beta2_dry * VI^2)) /
  #     ((alpha-wet - alpha_dry) + (beta1_wet - beta1_dry)*VI + (beta2_wet - beta2_dry)*VI^2)}

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

  STR_dry <- a_dry + b1_dry * VI + b2_dry * VI^2
  STR_wet <- a_wet + b1_wet * VI + b2_wet * VI^2
  W <- (STR_dry - STR) / (STR_dry - STR_wet)

  return(W)
}
