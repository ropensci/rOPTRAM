#' @title Prepare plot of transect line
#' @description Calculates monthly median of both NDVI and STR,
#' and plots these median points.
#' Typically over one season/year.
#' A bounding box enclosing all monthly points is added to the plot.
#'
#' @param from_date, string, Date for start of season, formatted as 'YYYY-MM-DD'.
#' @param to_date, string, Date for end of season.
#' @param output_dir, string, Full path to rOPTRAM Output_dir,
#' which contains subdirectories of STR and VI image files.
#' These directories are prepared and populated in advance by `optram_acquire_s2()`
#' @param save, boolean, If TRUE (default) then save plot to output_dir.
#' @return ggplot2 plot
#' @export
#'
#' @examples
#' Output <- file.path(system.file(package = "rOPTRAM"), "extdata")
#' pl = plot_transect(from_date = "2022-11-01", to_date = "2023-10-01", output_dir = Output)
#' pl
#'
#' @note
#' This function relies on previously downloaded images for preparing an `rOPTRAM` model.
#' This would normally be done with the `optram_acquire_s2()` function,
#' or the wrapper function `optram()`. The downloaded VI and STR images are stored in: `output_dir/NDVI` and `output_dir/STR`. The transect plot is saved as a png file into `output_dir` and the ggplot2 object is returned for additional styling, etc.
#'
plot_transect <- function(from_date, to_date, output_dir, save = TRUE) {
  # Prepare data.frame of all raster images between chosen dates
  Month_name <- pt.x <- pt.y <- y <- NULL
  all_rasts <- list()
  all_medians <- list()
  index_names <- c("x", "y")

  # Get list of image files in both NDVI and STR subdirectories
  VI_name <- getOption("optram.veg_index")
  img_dirs <- c(file.path(output_dir, VI_name), file.path(output_dir, "STR"))
  for (dir in img_dirs) {
    files <- list.files(dir, pattern = ".*tif$", full.names = TRUE)
    rast_list <- lapply(files, function(f) {
      fname <- basename(f)
      date_str <- unlist(strsplit(fname, "_"))[2]
      one_date <- lubridate::as_date(date_str)
      if (one_date >= from_date & one_date <= to_date) {
        one_rast_df = data.frame("Date" = one_date,
                                 "Index" = basename(dir),
                                 "Filepath" = f)
        return(one_rast_df)
      } else {
        return(NULL)
      }
      #print(paste("Found: ",length(rast_list)))
    })
    rast_list <- rast_list[!sapply(rast_list, is.null)]
    all_rasts <- append(all_rasts, rast_list)
  }

  # Calulate median monthly for both indices
  if (length(all_rasts) == 0) {
    # No images in this date range
    print(paste("No images available between: ", from_date, " and ", to_date))
    return(NULL)
  } else {
    rasts_df <- do.call(rbind, all_rasts)
    rasts_df['Month'] = lubridate::month(rasts_df$Date)
    rasts_df['Month_name'] = format(rasts_df$Date, "%b")

    # Loop over 12 months, create median raster for each index, for each month
    mon_nums <- unique(rasts_df$Month)
    for (x in seq_len(length(img_dirs))) {
      idx <- basename(img_dirs[x])
      idx_name <- index_names[x]
      month_list <- lapply(mon_nums, function(m){
        #print(paste("Month: ", m))
        monthly_df <- rasts_df[rasts_df$Month == m &
                                 rasts_df$Index == idx,]
        month_rast_list <- monthly_df$Filepath
        monthly_rast <- terra::rast(month_rast_list)
        # Create mew raster with median values of all rasters for this month
        monthly_merged <- terra::app(monthly_rast, 'median', na.rm = TRUE)
        monthly_median <- stats::median(terra::values(monthly_merged),
                                        na.rm = TRUE)
        df_1 <- data.frame("Month" = m,
                           Index = monthly_median,
                           "Month_name" = month.abb[m])
        return(df_1)
      })
      month_df <- do.call(rbind, month_list)
      names(month_df) <- c('Month', idx_name, "Month_name")
      all_medians[[x]] <- month_df
    }
    df <- do.call(cbind, all_medians)
    # Remove second 'Month' column
    df <- df[,-c(4,6)]
    df <- df[c("Month", "Month_name", 'x', 'y')]
  }

  # Now plot
  df$Month = factor(df$Month)
  df$Month_name = factor(df$Month_name, levels = (month.abb))

  # Prepare bounding box
  bx = data.frame("pt.x" = c(min(df$x), min(df$x), max(df$x), max(df$x), min(df$x)),
                  "pt.y" = c(min(df$y), max(df$y), max(df$y), min(df$y), min(df$y)))
  diag <- bx[c(1,3),]

  pl <- ggplot(data = df) +
    geom_point(aes(x=x, y=y, colour = Month_name), size = 6, shape = 19) +
    geom_path(aes(x=x, y=y, colour = Month_name, group = 1),
              linewidth = 1.5) +
    geom_path(data = bx, aes(x=pt.x, y=pt.y, group = 1),
              color = "darkgrey", linewidth = 1.0) +
    geom_line(data = diag, aes(x=pt.x, y=pt.y, group = 1),
              color = "darkgrey", linetype = 'dotdash', linewidth = 1.0) +
    labs(title = paste("Transect plot ", from_date, 'to', to_date),
         x = "VI values", y = "STR values") +
    theme_bw() +
    theme(plot.title = element_text(size=18, face = "bold"),
          plot.subtitle = element_text(size = 16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 16))

  if (save) {
    ggsave(plot = pl, filename = file.path(
      output_dir, paste0("transect_", from_date, "_", to_date, ".png")))
  }
  return(pl)
}
