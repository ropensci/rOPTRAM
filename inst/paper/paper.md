---
title: "`rOPTRAM`: Implementation of the OPTRAM Algorithm in `R`"
tags:
- soil moisture
- remote sensing
- "Sentinel-2"
- R
date: "01 March 2024"
output: pdf_document
authors:
- name: Micha Silver
  orcid: "0000-0002-1128-1325"
  equal-contrib: true
  affiliation: 1
  corresponding: true
- name: Arnon Karnieli
  orcid: "0000-0001-8065-9793"
  affiliation: 1
  equal-contrib: true
bibliography: bibliography.bib
affiliations:
- name: Ben Gurion University, Beer Sheva, Israel
  index: 1
---

## Abstract

While drylands supply a livelihood to much of the world's rural population, these arid and semi-arid areas are under increased pressure due to both growing demand and the current climate crisis. Maintaining a sustainable source of food for rural populations depends on reliable grazing, and the quality of grazing is in turn determined by soil moisture. Thus to ensure a sustainable food supply, soil moisture must be monitored. Classic soil moisture monitoring methods rely on sensors, inserted into the soil, that give accurate measurements at high temporal resolution, but at a point location.

However, answering the needs of populations that depend on extensive grazing lands requires a regional scale assessment of soil moisture. Point measurements, albeit accurate, do not afford the needed information to prepare for, or mitigate drought events at regional scale. To this end, remote sensing methods to estimate soil moisture have been developed. Among them, the OPTRAM model has been shown to accurately determine soil moisture over large areas. The `rOPTRAM` package in `R` has implemented that model allowing researchers and practitioners to monitor grazing potential at regional scale and long time intervals.

## Introduction

Across the African Sahel tens of millions of people depend on herds of grazing animals for food security [@mbow_land_2021; @kusserow_desertification_2017]. A time series of soil moisture measurements can serve to determine the quality and potential for grazing in arid and semi-arid regions. By following seasonal variations in soil moisture, forecasts for preferred grazing lands can be prepared. *In situ* volumetric soil content typically is measured using Time Domain Reflectrometer (TDR) sensors with a high temporal resolution [@kirkham_time_2014]. TDR sensors are recognized to be very accurate, and can collect for long periods. However these data are point measurements, and cannot cover the extensive areas needed for determining regional scale grazing potential. Cosmic Ray Neutron Scanner (CRNS) technology can acquire soil moisture measurements also at high temporal resolution, with a larger spatial scale than TDR, covering a few hectares. These instruments are, however, quite expensive, and less accurate than TDR spot measurements [@davies_optimal_2022; @schron_improving_2017]. Thus soil moisture measurements at regional scale, and with long term trend estimates are lacking. Populations living in drylands need high resolution regional soil moisture predictions to prepare for and mitigate droughts.

A novel physical-based model, OPtical TRApezoid Model (OPTRAM) developed by @sadeghi_optical_2017, that uses remote sensing imagery, was recently proposed and validated [@longo-minnolo_stand-alone_2022] to address the need to estimate soil moisture over vast areas in watershed and regional scales. OPTRAM is based on the well known physical relationship between soil moisture and land surface temperature (LST) [@lambin_surface_1996]. Less than a decade ago @sadeghi_optical_2017 showed that shortwave infrared (SWIR) Transformed Reflectance (STR) can replace the thermal band needed to derive LST. Thus the model can now be applied to various earth observation systems with visible, near-infrared, and SWIR bands such as Sentinel-2 or Landsat [@ambrosone_retrieving_2020; @dubinin_using_2020].

`rOPTRAM` produces a large dataset of pixel values of two satellite based raster layers: a vegetation index (VI), such as Normalized Difference Vegetation Index (NDVI) and the STR layer. All pairs of pixel values, at all acquired image dates are plotted as a scatter plot. Then regression lines are extracted at both the upper ("wet") and lower ("dry") bounds of the scatter plot. The slopes and intercepts of these two regression lines are the model coefficients, used to derive a spatially explicit soil moisture map. This soil moisture map is calculated, following @sadeghi_optical_2017, using Equation @eq-sm.

## Algorithm

To delineate the upper and lower, "wet" and "dry" bounds of the VI/STR scatterplot, the `rOPTRAM` package uses the following approach. Sentinel-2 images are acquired, through the `CDSE` package [@karaman_cdse_2023], clipped to the study area, and for the user-specified time range. The API request sent to the Copernicus DataSpace Ecosystem[^1] prepares both VI and STR indices. All pixel values for both indices, and for all images are collected into a table, and plotted as a scatterplot. The VI axis is divided, programatically, into a series of small intervals. Then the top and bottom 5% quartiles of STR values are found for each of these intervals. Then the pairs of upper quartile values together with the VI values for each interval comprise the points along the "wet" trapezoid edge. Similarly the bottom quartile values, paired with VI values make up the "dry" trapezoid edge.


[^1]: https://dataspace.copernicus.eu/
#### Linear regression fit of trapezoid edges

<span id="eq-exp1dry">$$
STR_{dry} = i_{dry} + s_{dry} \cdot VI
 \qquad(1)$$</span>

<span id="eq-exp1wet">$$
STR_{wet} = i_{wet} + s_{wet} \cdot VI
 \qquad(2)$$</span>

where:
$i_{wet}, i_{dry}$ are the regression line intercepts, and
$s_{wet}, s_{dry}$ are the slopes

Then soil moisture can be derived from:

<span id="eq-sm">$$
W = \frac{STR - STR_{dry}}{STR_{wet} - STR_{dry}}
 \qquad(3)$$</span>
 

#### Second order polynomial fit of trapezoid edges

<span id="eq-exp1dry">$$
STR_{dry} = \alpha_{dry} + \beta1_{dry} \cdot VI + \beta2_{dry} \cdot VI^2
 \qquad(1)$$</span>

<span id="eq-exp1wet">$$
STR_{wet} = \alpha_{wet} + \beta1_{wet} \cdot VI + \beta2_{wet} \cdot VI^2
 \qquad(2)$$</span>

as

<span id="eq-exp2">$$
W = \frac{STR - (\alpha_{dry} + \beta1_{dry} \cdot VI + \beta2_{dry} \cdot VI^2)}{(\alpha_{wet} + \beta1_{wet} \cdot VI + \beta2_{wet} \cdot VI^2) - (\alpha_{dry} + \beta1_{dry} \cdot VI + \beta2_{dry} \cdot VI^2)}
 \qquad(4)$$</span>


## Examples

#### Setup

Define directories and load required packages

``` r
Output_dir <- tempdir()
# Edit here...
work_dir <- "."
GIS_dir <- file.path(work_dir, "GIS")
aoi_file <- file.path(GIS_dir, "lachish.gpkg")

# load libraries
remotes::install_gitlab("rsl-bidr/rOPTRAM")
pkgs <- c("knitr", "ggplot2", "rOPTRAM", "jsonlite")
invisible(lapply(pkgs, require, character.only = TRUE))
# model parameters
veg_index <- "NDVI"
from_date <- "2022-09-01"
to_date <- "2023-04-01"
max_cloud <- 5
```

#### Run model, linear trapezoid fitting

Acquiring images from Copernicus DataSpace requires registering and setting up an OAuth client. Refer to the documentation.

```r
BOA_list <- optram_acquire_s2(aoi_file = aoi_file,
                          from_date = from_date,
                          to_date = to_date, 
                          max_cloud = max_cloud,
                          veg_index = veg_index, 
                          output_dir = GIS_dir,
                          remote = "scihub")
VI_dir <- file.path(GIS_dir, veg_index)
VI_list <- list.files(VI_dir, full.names = TRUE)
STR_dir <- file.path(GIS_dir, "STR")
STR_list <- list.files(STR_dir, full.names = TRUE)
VI_STR_df <- optram_ndvi_str(STR_list, VI_list,
                             output_dir = GIS_dir,
                             rm.low.vi = TRUE,
                             rm.hi.str = TRUE)
rmse <- optram_wetdry_coefficients(
  VI_STR_df,
  aoi_file = aoi_file,
  output_dir = GIS_dir,
  vi_step = 0.005,
  trapezoid_method = "linear",
  edge_points = TRUE)
knitr::kable(rmse, caption = "RMSE values for wet/dry fitted lines")
```

<img src="../GIS/trapezoid_lachish_linear.png"
style="width:12cm" alt="Hi density trapezoid plot" />

#### Second run: polynomial fitted curves

```r
rmse <- optram_wetdry_coefficients(
  VI_STR_df,
  aoi_file = aoi_file
  output_dir = GIS_dir,
  vi_step = 0.005,
  trapezoid_method = "polynomial",
  edge_points = TRUE)
knitr::kable(rmse, caption = "RMSE values for wet/dry fitted lines")
```


<img src="../GIS/trapezoid_lachish_polynomial.png"
style="width:12cm" alt="Hi density trapezoid plot" />

## Acknowledgements



## References

