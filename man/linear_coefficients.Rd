% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trapezoid_utilities.R
\name{linear_coefficients}
\alias{linear_coefficients}
\title{Utility Function to Prepare Linear Regression Edges of Trapezoid}
\usage{
linear_coefficients(df, output_dir)
}
\arguments{
\item{df, }{data.frame, values of VI and STR along edges of trapezoid}

\item{output_dir, }{string, path to save coefficients CSV file}
}
\value{
df, data.frame, the trapezoid line edge points
with fitted wet/dry values added
}
\description{
Called by \code{\link[rOPTRAM]{optram_wetdry_coefficients}}
to prepare linear regression line along trapezoid edges
Calculates the intercept and slope of both wet and dry edges
Not exported
}
\note{
Three CSV files are saved:
\itemize{
\item the regressions coefficients,
\item the trapezoid edge points
\item RMSE of the fitted curve
}
}
\examples{
\dontrun{
edges_file <- system.file("extdata/trapezoid_edges.csv",
                           package = "rOPTRAM")
df <- read.csv(edges_file)
output_dir <- tempdir()
coeffs <- linear_coefficients(df, output_dir)
coeffs
}
}
