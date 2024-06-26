% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trapezoid_utilities.R
\name{polynomial_soil_moisture}
\alias{polynomial_soil_moisture}
\title{Calculate Soil Moisture, Polynomial Fitted Curve}
\usage{
polynomial_soil_moisture(coeffs, VI, STR)
}
\arguments{
\item{coeffs, }{list, 6 trapezoid coefficients}

\item{VI, }{terra rast, the vegetation index raster}

\item{STR, }{terra rast, the STR raster}
}
\value{
rast, soil moisture grid
}
\description{
Prepare soil moisture grid from STR and VI images
for a single date, based on polynomial function fitted to trapezoid edges.
}
\note{
This function is used after preparing the OPTRAM model coefficients with:
\code{\link[rOPTRAM]{optram_wetdry_coefficients}}. Typically a new image date,  (that was not used for preparing the model),
will be referenced in the \code{img_date} parameter.
The resulting soil moisture raster is saved to \code{output_dir}
This function implements an polynomial fitted curve, following:
Ma, Chunfeng, Kasper Johansen, and Matthew F. McCabe. 2022.
“Combining Sentinel-2 Data with an Optical-Trapezoid Approach to Infer within-Field Soil Moisture Variability
and Monitor Agricultural Production Stages.”
Agricultural Water Management 274 (December): 107942.
\doi{https://doi.org/10.1016/j.agwat.2022.107942}.
}
\examples{
img_date <- "2023-03-11"
VI_dir <- system.file("extdata", "NDVI", package = "rOPTRAM")
STR_dir <- system.file("extdata", "STR", package = "rOPTRAM")
optram_options("trapezoid_method", "polynomial")
SM <- optram_calculate_soil_moisture(img_date,
                          VI_dir, STR_dir,
                          data_dir = tempdir())

}
