#' Derive coefficients of slope and intercept
#' for both wet and dry trapezoid lines
#'
#' @param full_df, data.frame of STR and NDVI values
#' @param step, float
#'
#' @return list of float, coefficients of wet-dry trapezoid
#' @export
#' @importFrom stats complete.cases lm quantile
#' @importFrom utils read.csv write.csv
#' @examples print("Running wetdry_coefficients.R")
#'
WetDry_Coefficients <- function(full_df, step=0.001){
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
  NDVI_min_max <- round(quantile(full_df$NDVI, c(0.5, 0.95)) , 2)
  NDVI_series <- seq(NDVI_min_max[[1]], NDVI_min_max[[2]], step)
  print(paste("NDVI series length:", length(NDVI_series)))
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
    Qs <- quantile(interval_df$STR, c(0.1, 0.9), na.rm=TRUE)
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
  write.csv(STR_NDVI_df,
            file.path(Output_dir, "STR_NDVI_df.csv"),
            row.names = FALSE)
  # Run linear regression between STR and NDVI
  # to determine the intercept and slope for both wet and dry data
  wet_fit <- lm(STR_wet ~ NDVI, data=STR_NDVI_df)
  dry_fit <- lm(STR_dry ~ NDVI, data=STR_NDVI_df)
  i_wet <- wet_fit$coefficients[[1]]
  s_wet <- wet_fit$coefficients[[2]]
  i_dry <- dry_fit$coefficients[[1]]
  s_dry <- dry_fit$coefficients[[2]]
  coeffs <- data.frame("intercept_dry"=i_dry, "slope_dry"=s_dry,
                       "intercept_wet"=i_wet, "slope_wet"=s_wet)
  write.csv(coeffs, coeffs_file, row.names=FALSE)

  return(coeffs)
}
