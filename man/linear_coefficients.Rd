% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trapezoid_utilities.R
\name{linear_coefficients}
\alias{linear_coefficients}
\title{Utility Function to prepare linear regression edges of trapezoid}
\usage{
linear_coefficients(df, output_dir)
}
\arguments{
\item{df, }{data.frame, values of VI and STR along edges of trapezoid}

\item{output_dir, }{string, path to save coefficients csv file}
}
\value{
coeffs, data.frame of 2 linear regression coefficients
for both wet and dry edges
}
\description{
Called by \code{optram_wetdry_coefficients()}
to prepare linear regression line along trapezoid edges
Calculates the intercept and slope of both wet and dry edges
Not exported
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