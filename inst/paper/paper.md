---
title: 'rOPTRAM: Implementation of the OPTRAM Algorithm in R'
tags:
  - soil moisture
  - remote sensing
  - Sentinel-2
  - R
  - grasslands
authors:
  - name: Micha Silver
    orcid: 0000-0002-1128-1325
    affiliation: 1
  - name: Dong Zhe
    orcid: 0009-0007-5298-8355
    affiliation: 1
  - name: Arnon Karnieli
    orcid: 0000-0001-8065-9793
    affiliation: 1
affiliations:
 - name: Blaustein Institutes for Desert Research, Ben Gurion University, Israel
   index: 1
date: 07 June 2024
bibliography: bibliography.bib
---

# Summary

Soil moisture at the Earth’s surface is essential in many hydrological, pedological, and biological processes. Earth observations from satellites have been recognized as the most efficient and reliable means for assessing soil water content.  The satellite-derived OPTRAM model has been shown to determine soil moisture over large areas accurately. The overarching goal of the current project was to program the original OPTRAM algorithm, with some improvements, using the open-source ``R`` language. The spectral data were obtained from different spaceborne systems to calculate several vegetation indices, and the model was adjusted to various SM conditions. The ``rOPTRAM`` package allows researchers and practitioners to monitor soil moisture contents at a regional scale and long-time intervals.

# Statement of Need

Soil moisture (SM) at the Earth’s surface is essential in many hydrological, pedological, and biological processes. Pasture is the most common land use, where grazing livestock spreads over about 77% of the global agricultural land, and support billions of inhabitants [@ramankutty_farming_2008; @maestre_grazing_2022]. Therefore SM is a critical resource in these regions for the growth of palatable plants, pasture quality, and rangeland health [@mbow_land_2021; @kusserow_desertification_2017]. A time series of SM measurements can serve to determine the quality and potential for grazing. By following seasonal variations in SM, forecasts for preferred grazing lands can be prepared. *In-situ* volumetric soil content is typically measured using Time Domain Reflectometer (TDR) sensors with a high temporal resolution [@kirkham_time_2014]. TDR sensors have been recognized to be very accurate, and can function for long periods. However these data are point measurements, and cannot cover the extensive areas needed for determining regional scale grazing potential. Cosmic Ray Neutron Scanner (CRNS) technology can also acquire SM measurements at high temporal resolution, with a larger spatial scale than TDR, covering up to 30 hectares. These latter instruments are, however, quite expensive, and less accurate than TDR spot measurements [@davies_optimal_2022; @schron_improving_2017]. To overcome the locality issue, Earth Observations from satellites have been accepted as the most efficient and reliable means for assessing the Earth’s resources, including SM, in isolated and inaccessible regions due to its large-scale coverage and repeatability for the last decades. 

Less than a decade ago, @sadeghi_optical_2017 showed that shortwave infrared (SWIR) Transformed Reflectance (STR) is applicative to assess SM. They developed a novel physical-based model, OPtical TRApezoid Model (OPTRAM) using designated spectral bands of remote sensing imagery. OPTRAM was recently validated [@longo-minnolo_stand-alone_2022] to address the need to estimate SM over vast areas in watershed and regional scales. Consequently the model can now be applied to various earth observation systems, such as Sentinel-2 or Landsat, with visible, near-infrared, and SWIR bands. [@ambrosone_retrieving_2020; @dubinin_using_2020].

The overarching goal of the current project was to program the original OPTRAM algorithm, with some improvements, using the open-source ``R`` language. The spectral data were obtained from different spaceborne systems to calculate several vegetation indices, and the model was adjusted to various SM conditions.  


# Algorithm

``rOPTRAM`` produces a large dataset of pixel values of two satellite-derived raster layers: a vegetation index (VI), such as Normalized Difference Vegetation Index (NDVI) and the SWIR transformed reflectance (STR). All pairs of pixel values at all acquired image dates are plotted as a scatter plot. Then regression lines are extracted at both the upper ("wet") and lower ("dry") bounds of the scatter plot. The slopes and intercepts of these lines are the model coefficients used to derive a spatially explicit soil moisture map. This soil moisture map is calculated using the equations developed by @sadeghi_optical_2017. In his original work, @sadeghi_optical_2017 visually examined the scatterplot to locate the trapezoid edges.

The proposed ``rOPTRAM`` package, on the other hand, delineates the upper and lower, "wet" and "dry" bounds of the STR/VI scatterplot programmatically, through the following approach. Sentinel-2 images are acquired using the ``CDSE`` package [@karaman_cdse_2023], clipped to the study area, and for the user-specified time range. The Application Programming Interface (API) request sent to the Copernicus DataSpace Ecosystem[^1] (CDSE) prepares both VI and STR indices. All pixel values for both indices, and all images along the time series are collected into a table and plotted as a scatterplot. The VI axis of the scatterplot is divided, programmatically, into a series of small intervals, and a subset of the STR values, within that narrow interval of VI is extracted. Outlier STR values are removed at this stage based on the accepted 1.5 times Inter Quartile Range (IQR) method. Then, among the remaining values, the top and bottom 2% quartiles of these STR values are found for each interval. The upper quartile values are paired with the VI values for each interval, thus collecting points along the "wet" trapezoid edge. Similarly the bottom quartile values, paired with VI values, make up the "dry" trapezoid edge. Each of these two sets, typically consisting of 50 to 100 points, delineates trapezoid edges, thus offering a mathematically robust and repeatable implementation of the OPTRAM model.

One of three possible equations is fitted to each of these "wet" and "dry" sets of trapezoid edges (Figure \autoref{fig:diag}. A linear Ordinary Least Squares (OLS) regression line is fitted to each set of points in the most straightforward case. The intercept and slope of these lines give the coefficients for calculating soil water content. Two additional fitted options are implemented in ``rOPTRAM``: exponential and second-order polynomial. For OLS fitted curves, two coefficients are derived for each line, the slope and intercept. Similarly, the exponential fitted curve requires two coefficients, the intercept and the multiplier of VI in the exponential term. A polynomial fit, on the other hand, consists of 3 coefficients, the intercept, and the coefficents for the first-order and second-order terms. 

![OPTRAM schematic diagram.\label{fig:diag}](GIS/OPTRAM_diagram.jpg)

In all cases, the fitting function returns the root mean square error (RMSE) of the fitted line to the original 2% quartile trapezoid edges, enabling evaluation of the fitted result.

[^1]: https://dataspace.copernicus.eu/

### Linear regression fit of trapezoid edges

$$  STR_{dry} = i_{dry} + s_{dry} \cdot VI $$

$$ STR_{wet} = i_{wet} + s_{wet} \cdot VI $$

where:
$i_{wet}, i_{dry}$ are the regression line intercepts, and
$s_{wet}, s_{dry}$ are the slopes

Then soil moisture can be derived from:

$$  W = \frac{STR - STR_{dry}}{STR_{wet} - STR_{dry}} $$

### Exponential curve fit of trapzoid edges

Exponential fitted curves for the "wet" and "dry" edges can be expressed as:

$$  STR_{dry} = i_{dry} \cdot exp(s_{dry} \cdot VI) $$

$$  STR_{wet} = i_{wet} \cdot exp(s_{wet} \cdot VI)  $$

and in this case, soil moisture is derived as:

$$  W = \frac{STR - (i_{dry} \cdot exp(s_{dry} \cdot VI))}{(i_{wet} \cdot exp(s_{wet} \cdot VI)) - (i_{dry} \cdot exp(s_{dry} \cdot VI))} $$

 
### Second order polynomial fit of trapezoid edges

Polynomial fitted regression lines for the "wet" and "dry" edges can be expressed as:

$$  STR_{dry} = \alpha_{dry} + \beta1_{dry} \cdot VI + \beta2_{dry} \cdot VI^2 $$

$$  STR_{wet} = \alpha_{wet} + \beta1_{wet} \cdot VI + \beta2_{wet} \cdot VI^2 $$


and in this case, soil moisture is derived as:

$$ W = \frac{STR - (\alpha_{dry} + \beta1_{dry} \cdot VI + \beta2_{dry} \cdot VI^2)}{(\alpha_{wet} + \beta1_{wet} \cdot VI + \beta2_{wet} \cdot VI^2) - (\alpha_{dry} + \beta1_{dry} \cdot VI + \beta2_{dry} \cdot VI^2)} $$



# Examples

### Setup

Define directories and load required packages

``` r
Output_dir <- tempdir()
# Edit here...
work_dir <- system.file("paper", package = "rOPTRAM") 
GIS_dir <- file.path(work_dir, "GIS")
aoi_file <- file.path(GIS_dir, "lachish.gpkg")

# load libraries
remotes::install_github("ropensci/rOPTRAM")
pkgs <- c("knitr", "ggplot2", "rOPTRAM", "jsonlite")
invisible(lapply(pkgs, require, character.only = TRUE))
# model parameters
veg_index <- "NDVI"
from_date <- "2022-09-01"
to_date <- "2023-04-01"
max_cloud <- 5
```

### Run model, linear trapezoid fitting

Acquiring images from Copernicus DataSpace requires registering and setting up an OAuth client. Refer to the documentation.

```r
BOA_list <- optram_acquire_s2(aoi_file = aoi_file,
                          from_date = from_date,
                          to_date = to_date, 
                          max_cloud = max_cloud,
                          veg_index = veg_index, 
                          output_dir = Output_dir,
                          remote = "scihub")
VI_dir <- file.path(Output_dir, veg_index)
VI_list <- list.files(VI_dir, full.names = TRUE)
STR_dir <- file.path(Output_dir, "STR")
STR_list <- list.files(STR_dir, full.names = TRUE)
VI_STR_df <- optram_ndvi_str(STR_list, VI_list,
                             output_dir = Output_dir,
                             rm.low.vi = TRUE,
                             rm.hi.str = TRUE)

rmse <- optram_wetdry_coefficients(
  VI_STR_df,
  aoi_file = aoi_file,
  output_dir = GIS_dir,
  vi_step = 0.005,
  trapezoid_method = "linear",
  edge_points = TRUE)
```

||RMSE wet edge| RMSE dry edge
|:----------|----------:|----------:|
|Linear fit: |0.2460|0.0955|

![Linear fitted trapezoid plot.\label{fig:trapezoid_linear}](GIS/trapezoid_lachish_linear.png)

### Second run: polynomial fitted curves

```r
rmse <- optram_wetdry_coefficients(
  VI_STR_df,
  aoi_file = aoi_file,
  output_dir = GIS_dir,
  vi_step = 0.005,
  trapezoid_method = "polynomial",
  edge_points = TRUE)
```

||RMSE wet edge|RMSE dry edge|
|:--------------|---------:|---------:|
|Polynomial fit:| 0.1490| 0.0931|

![Polynomial fitted trapezoid plot.\label{fig:trapezoid_poly}](GIS/trapezoid_lachish_polynomial.png)

# Future work

Ongoing package development will include implementing a set of model options throughout the package. This will enable users to define and execute various model scenarios efficiently. Options will include:

* choice of vegetation index (i.e. SAVI, NDVI);
* selecting a SWIR band (1600 or 2200 nm in Sentinel-2 imagery);
* acquiring seasonal imagery or the full time range;
* specifying which curve fitting function to use;
* and plotting options for viewing the VI-STR scatterplot.
	
All options will have a default value, and users will be able to define other choices at the start of a workflow. This approach will simplify function calls, and allow flexibility for users.

# Acknowledgements

The authors wish to thank the two reviewers, Lauren O'Brien and Harry Eslick, whose comments substantially improved the code in this package for submission to ROpenSci.

# References

