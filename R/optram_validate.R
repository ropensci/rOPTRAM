#' @title Validate OPTRAM model with in situ data
#' @description Validation of the OPTRAM model is performed by reading a CSV file
#' of ground truth soil moisture data, typically from TDR sensors.
#' The CSV data file represents measurements over a time series, for one location.
#' The predicted soil moisture values from the model are extracted at the sensor location
#' from a time series of OPTRAM model prediction rasters.
#' (derived from the `optram_calculate soil_miosture()` function)
#' The ground truth measurements are correlated with the model predictions
#' and an optional scatterplot with regression line are plotted.
#' @param data_file, string, full path to CSV file of in situ measurements. See Note below.
#' @param model_output_dir, string, path to directory of rasters from:
#' `optram_calculate soil_moisture()` function
#' @param TDR_file, string, full path to spatial file of TDR sensor.
#'  Either geopackage, shapefile, or other spatial data format.
#' @param TDR_layer, string, layer name of TDR location
#'  (required only if the spatial file is geopackage).
#' @param output_dir, string, where to save data.frame of soil moisture values
#' and (optional) plot.
#' @param show_plot, boolean, whether to prepare regression plot, default FALSE
#' @return sm_df, data.frame of soil moisture values, in situ and predicted
#' This data is also saved to and *.rds file.
#' @export
#' @examples 
#' #' print("Running optram_validate.R")
#' @note The data file should contain two columns:
#' the first column must be date time in ISO format: "YYYY-MM-DD HH:MM:SSZ"
#' The second column must be the Volumetric Water Content in percent
#' (all other columns are ignored) 

optram_validate <- function(data_file,
                            model_output_dir,
                            TDR_file,
                            TDR_layer = NULL,
                            output_dir = tempdir(),
                            show_plot = FALSE) {

    # Check inputs
    if (is.null(data_file) | ! file.exists(data_file)) {
        warning("No TDR data file:", data_file, "Exiting...")
        return(NULL)
    }
    if (is.null(TDR_file)) {
        warning("No TDR location specified.",
                "Please set TDR_file, as path to spatial file of TDR location")
        return(NULL)
    }
    model_output_list <- list.files(model_output_dir,
                                    pattern = "soil_moisture.*tif$",
                                    full.names = TRUE)
    if (length(model_output_list) == 0) {
        warning("No model output raster files available. Exiting...")
        return(NULL)
    }

    tdr <- terra::vect(TDR_file, layer = TDR_layer)

    # The Sentinel 2 satellite is positioned in a sun-synchronous orbit.
    # The orbit is chosen such that local mean time (crossing the equator)
    # on the descending orbit is approximately 10:30.
    # (changes slightly over the year)
    # The orbital period is 100 minutes, thus:
    # for each degree north of the equator the "fly-by" is
    # 100 / 360 = 0.28 minutes earlier
    # and for each degree south of the equator, 0.28 minutes after 10:30

    lat <- terra::crds(terra::project(tdr, "epsg:4326"))[2]
    # Time delta for this TDR location
    diff_secs <- 60 * (-0.28 * lat)

    # Reproject the tdr locations to the CRS of the soil moisture rasters
    sm_1 <- terra::rast(model_output_list[1])
    tdr <- terra::project(tdr, sm_1)
    # The data file is assumed to be formatted with two columns
    # The first contains date time (UTC time zone) as "YYYY-MM-DD HH:MM:SSZ"
    # The second column contains Volumetric water content in %
    tdr_data <- utils::read.csv(data_file)
    names(tdr_data) <- c("datetime", "vol_water_content")
    # Loop over time series of soil moisture rasters
    sm_df_list <- lapply(model_output_list, function(f) {
        # Check that the TDR locations is actually within the extent
        # of the soil moisture raster
        sm <- terra::rast(f)
        overlap <- terra::intersect(tdr, terra::ext(sm))
        if (is.na(terra::geom(overlap)[1])) {
            return(NULL)
        }
        # Assumes that the file names of soil moisture rasters follows
        # `optram_calculate_soil_moisture()`: "soil_moisture_YYYY-MM-DD.tif"
        f_base <- tools::file_path_sans_ext(basename(f))
        date_str <- unlist(strsplit(f_base, "_"))[3]
        # THe local mean solar time for Seninel is 10:30 UTC
        # Add (subtract) the difference based on latitude
        sm_date <- as.POSIXct(paste(date_str, "10:30:00"), tz="UTC") + diff_secs
        # Get TDR data for this date
        idx <- findInterval(sm_date, tdr_data$datetime)
        sm_tdr <- tdr_data$vol_water_content[idx]

        sm_model <- terra::extract(sm, tdr)
        # Get date of image, and construct datetime
        sm_df_1 <- data.frame("Date_Time" = sm_date,
                            "SM_TDR" = sm_tdr,
                            "SM_Model" = sm_model)
        return(sm_df_1)
    })
    sm_df <- do.call(rbind, sm_df_list)
    if (nrow(sm_df) == 0) {
        warning(
            "No overlap between TDR locations and soil moisture raster",
            "\nExiting...")
    }
    optram_r.squared <- stats::cor(sm_df$SM_TDR, sm_df$SM_Model)^2
    print(paste("OPTRAM correlation (R^2): ", optram_r.squared))

    if (show_plot) {
        plot_correlation(sm_df, output_dir)
    }
    return(sm_df)
}


#' @title Plot validation regression line
#' @description Prepare a scatterplot of in situ measurements of soil moisture
#' against model predicted values at TDR TDRs.
#' and add regression line 
#' @param sm_df, data.frame, values of in situ and model predicted soil moisture
#' over a range of dates
#' @param output_dir, string, full path to save png file of plot 
#' @return png_file, string, full path to png file of plot
#' @export 
#' @examples 
#' print("Running correlation_plot()")
#'

plot_correlation <- function(sm_df, output_dir) {
    min_date <- min(sm_df$Date_Time)
    max_date <- max(sm_df$Date_Time)
    ttl <- "TDR-Model soil moisture correlation"
    subttl <- paste(strftime(min_date, "%d-%m-%Y"),
                 strftime(max_date, "%d-%m-%Y"))
    png_file <- paste("tdr_model_correlation",
                      strftime(min_date, "%Y%m%d"),
                      strftime(max_date, "%Y%m%d"), sep="_")
    png_path <- file.path(output_dir, paste0(png_file, ".png"))

    pl <- ggplot2::ggplot(sm_df, aes(x = SM_TDR, y = SM_Model)) +
        geom_point(color = "blue") +
        geom_smooth(method = "lm", se = TRUE)
        xlab("TDR measured soil moisture [%]") +
        ylab("Model predicted soil moisture [%]") +
        ggtitle(ttl, subtitle = subttl)

    ggsave(png_path, plot = pl)

    # Also display plot
    pl
}