# call optram_landsat
library(devtools)
load_all()

landsat_dir = "C:/Users/Natalya/Downloads/landsat_oPTRAM"
aoi_file <- "D:/rOPTRAM/aoi/migda_perimeter.shp"
vi = "NDVI"
LC_output_dir = "D:/rOPTRAM/derived_products"
data_output_dir = "D:/rOPTRAM/output"

#' @return coeffs, list, the derived trapezoid coefficients

coeffs <- optram_landsat(landsat_dir,
                aoi_file,
                vi = 'NDVI',
                LC_output_dir,
                data_output_dir)

print(coeffs)
