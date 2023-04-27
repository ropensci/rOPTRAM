
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rOPTRAM

<!-- badges: start -->

[![Project Status:
WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

`rOPTRAM` implements The OPtical TRapezoid Model (OPTRAM) to derive soil
moisture based on the linear relation between a vegetation index,
i.e. NDVI, and Land Surface Temperature (LST).

The Short Wave Infra-red (SWIR) band is used as a proxy for LST. The
SWIR band is transformed to Swir Transformed Reflectance (STR).

A scatterplot of NDVI vs. STR is used to produce wet and dry linear
regression lines, and the slope/intercept coefficents of these lines
comprise the trapezoid. These coefficients are then used on a new
satellite image to determine soil moisture.

See: Sadeghi et al. (2017), Burdun et al. (2020), Ambrosone et al.
(2020)

## Installation

`rOPTRAM` resides on gitlab. You can install the development version of
rOPTRAM like so:

``` r
# Install remotes package
if (! require("remotes")) {
  install.packages("remotes")
}
remotes::install_gitlab("rsl-bidr/roptram")
```

#### Suggested prerequisites

This package works with the R package `sen2r`. To install that package
you will need some system dependencies. On Debian and derivitives please
install in advance: - jq libv8-dev libjq-dev - libprotobuf-dev
protobuf-compiler

Install the google cloud CLI to allow acquiring images directly from
Google. For installation instructions see:
<https://cloud.google.com/sdk/docs/install>

## Example

This is a basic example which shows how to: - retrieve Sentinel 2
imagery for a specific area of interest - covering a date range -
preprocess the imagery to obtain a vegetation index and STR band - use
these to derive coefficients of slope and intercept for the OPTRAM
trapezoid

``` r
library(rOPTRAM)
# basic example
geojson_file <- system.file("extdata", "migda_9.geojson", package="rOPTRAM")
coeffs <- rOPTRAM::optram(
    aoi = geojson_file,
    from_date = "2021-01-01", to_date = "2021-03-30",
    max_cloud = 10,
    vi = "NDVI")
coeffs
```

## Note

In order to download Sentinel 2 images, the `sen2r` package is used:

Ranghetti et al. (2020)

That package must be installed in advance in order to run the `optram()`
wrapper function.

If, on the other hand, Sentinel 2 imagery has been downloaded in
advance, then `sen2r` is not necessary. Instead, the following workflow
can be used: - call `optram_safe()` to prepare NDVI and STR rasters -
call `optram_calculate_str()` to calculate SWIR Transform - call
`optram_ndvi_str()` to build a data.frame of pixel values - call
`optram_wetdry_coefficients()` to derive slope and intercept of the
trapezoid.

## Meta

-   Please report any issues on
    [gitlab](https://gitlab.com/rsl-bidr/roptram/-/issues)
-   Anyone interested in collaborating is invited to “sign up” by
    contacting the maintainers.
-   This package is released with a [Contributor Code of
    Conduct](https://github.com/ropensci/.github/blob/master/CODE_OF_CONDUCT.md).
    By contributing to this project, you agree to abide by its terms.

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-ambrosone_retrieving_2020" class="csl-entry">

Ambrosone, Mariapaola, Alessandro Matese, Salvatore Filippo Di Gennaro,
Beniamino Gioli, Marin Tudoroiu, Lorenzo Genesio, Franco Miglietta, et
al. 2020. “Retrieving Soil Moisture in Rainfed and Irrigated Fields
Using Sentinel-2 Observations and a Modified OPTRAM Approach.”
*International Journal of Applied Earth Observation and Geoinformation*
89 (July): 102113. <https://doi.org/10.1016/j.jag.2020.102113>.

</div>

<div id="ref-burdun_satellite_2020" class="csl-entry">

Burdun, Iuliia, Michel Bechtold, Valentina Sagris, Annalea Lohila, Elyn
Humphreys, Ankur R. Desai, Mats B. Nilsson, Gabrielle De Lannoy, and Ülo
Mander. 2020. “Satellite Determination of Peatland Water Table Temporal
Dynamics by Localizing Representative Pixels of A SWIR-Based Moisture
Index.” *Remote Sensing* 12 (18): 2936.
<https://doi.org/10.3390/rs12182936>.

</div>

<div id="ref-ranghetti_sen2r_2020" class="csl-entry">

Ranghetti, Luigi, Mirco Boschetti, Francesco Nutini, and Lorenzo
Busetto. 2020. “Sen2r: An r Toolbox for Automatically Downloading and
Preprocessing Sentinel-2 Satellite Data.” *Computers & Geosciences* 139:
104473. <https://doi.org/10.1016/j.cageo.2020.104473>.

</div>

<div id="ref-sadeghi_optical_2017" class="csl-entry">

Sadeghi, Morteza, Ebrahim Babaeian, Markus Tuller, and Scott B. Jones.
2017. “The Optical Trapezoid Model: A Novel Approach to Remote Sensing
of Soil Moisture Applied to Sentinel-2 and Landsat-8 Observations.”
*Remote Sensing of Environment* 198 (September): 52–68.
<https://doi.org/10.1016/j.rse.2017.05.041>.

</div>

</div>
