#' Prepare dataframe of pairs NDVI and STR values
#'
#' @param STR_dir, path to STR raster file
#' @param NDVI_dir, path to NDVI raster file
#'
#' @return full_df, data.frame
#' @export
#'
#' @examples
#' print("Running ndvi_str_dataframe.R")
#'
NDVI_STR_DataFrame <- function(STR_dir, NDVI_dir){

  STR_file_list <- list.files(path=STR_dir, full.names = TRUE)
  STR_df_list <- lapply(STR_file_list, function(f){
    date_str <- unlist(strsplit(basename(f), split="_", fixed=TRUE))[2]
    STR <- terra::rast(f)
    STR_1_df <- as.data.frame(STR, xy=TRUE)
    names(STR_1_df) <- c("x", "y", "STR")
    STR_1_df['Date'] <- as.Date(date_str, format="%Y%m%d")
    return(STR_1_df)
  })
  STR_df <- do.call(rbind, STR_df_list)
  str_df_file <- file.path(Output_dir, "STR_data.rds")
  saveRDS(STR_df, str_df_file)
  print(paste("Saved:", nrow(STR_df), "rows to:", str_df_file))

  NDVI_file_list <- list.files(path=NDVI_dir, full.names = TRUE)
  NDVI_df_list <- lapply(NDVI_file_list, function(f){
    # Get image date
    date_str <- unlist(strsplit(basename(f), split="_", fixed=TRUE))[2]
    NDVI <- terra::rast(f)
    # Revert to original scale
    NDVI <- NDVI/10000.0
    NDVI_1_df <- as.data.frame(NDVI, xy=TRUE)
    names(NDVI_1_df) <- c("x", "y", "NDVI")
    NDVI_1_df['Date'] <- as.Date(date_str, format="%Y%m%d")
    return(NDVI_1_df)
  })
  NDVI_df <- do.call(rbind, NDVI_df_list)
  ndvi_df_file <- file.path(Output_dir, "NDVI_data.rds")
  saveRDS(NDVI_df, ndvi_df_file)
  print(paste("Saved:", nrow(NDVI_df), "rows to:", ndvi_df_file))

  full_df <- dplyr::full_join(STR_df, NDVI_df)
  full_df <- full_df[stats::complete.cases(full_df),]
  print(paste("Full data joined leaving", nrow(full_df), "rows"))
  return(full_df)
}
