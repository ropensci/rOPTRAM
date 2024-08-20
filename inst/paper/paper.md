---
title: 'rOPTRAM: Deriving Soil Moisture from Satellite Imagery in R'
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
  - name: Ron Beiden
    affiliation: 1
  - name: Zhe Dong
    orcid: 0009-0007-5298-8355
    affiliation: 1
  - name: Natalya Panov
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

Soil moisture at the Earth’s surface is essential in many hydrological, pedological, and biological processes. Earth observations from satellites have been recognized as the most efficient and reliable means for assessing soil water content.  The satellite-derived OPTRAM model has been shown to determine soil moisture over large areas accurately. Spectral data were obtained from different spaceborne systems to calculate several vegetation indices, and the model was adjusted to various SM conditions. The ``rOPTRAM`` package allows researchers and practitioners to monitor soil moisture contents at a regional scale and long-time intervals.

# Statement of Need

Soil moisture (SM) at the Earth’s surface is essential in many hydrological, pedological, and biological processes. Pasture is the most common land use, where grazing livestock spreads over about 77% of the global agricultural land, and supports billions of inhabitants [@ramankutty_farming_2008; @maestre_grazing_2022]. Therefore SM is a critical resource in these regions for the growth of palatable plants, pasture quality, and rangeland health [@mbow_land_2021; @kusserow_desertification_2017]. A time series of SM measurements can determine the quality and potential for grazing. By following seasonal variations in SM, forecasts for preferred grazing lands can be prepared. *In-situ* volumetric soil content is typically measured using Time Domain Reflectometer (TDR) sensors with a high temporal resolution [@kirkham_time_2014]. TDR sensors have been recognized to be very accurate and can function for long periods. However these data are point measurements and cannot cover the extensive areas needed for determining regional scale grazing potential. Cosmic Ray Neutron Scanner (CRNS) technology can also acquire SM measurements at high temporal resolution, with a larger spatial scale than TDR, covering up to 30 hectares. These latter instruments are, however, quite expensive, and less accurate than TDR spot measurements [@davies_optimal_2022; @schron_improving_2017]. To overcome the locality issue, Earth observations from satellites are the most efficient and reliable means for assessing the Earth’s resources, including SM due to its large-scale coverage and repeatability for the last decades. 

Less than a decade ago, @sadeghi_optical_2017 showed that shortwave infrared (SWIR) Transformed Reflectance (STR) is applicative to assess SM. They developed a novel physical-based model, OPtical TRApezoid Model (OPTRAM) using designated spectral bands of remote sensing imagery. OPTRAM was recently validated [@longo-minnolo_stand-alone_2022] to address the need to estimate SM over vast areas at regional scales. The model can now be applied to various earth observation systems, such as Sentinel-2 or Landsat, with visible, near-infrared, and SWIR bands. [@ambrosone_retrieving_2020; @dubinin_using_2020].

The overarching goal of the current project was to program the original OPTRAM algorithm, with some improvements, using the open-source ``R`` language. The spectral data were obtained from different spaceborne systems to calculate several vegetation indices, and the model was adjusted to various SM conditions.  


# Algorithm

``rOPTRAM`` produces a large dataset of pixel values of two satellite-derived raster layers: a vegetation index (VI), such as the Normalized Difference Vegetation Index (NDVI) and the SWIR transformed reflectance (STR). All pairs of pixel values at all acquired image dates are plotted as a scatter plot. Then regression lines are extracted at both the upper ("wet") and lower ("dry") bounds of the scatter plot. The slopes and intercepts of these lines are the model coefficients used to derive a spatially explicit soil moisture map. This soil moisture map is calculated using the equations developed by @sadeghi_optical_2017. In his original work, @sadeghi_optical_2017 visually examined the scatterplot to locate the trapezoid edges.

The new ``rOPTRAM`` package, on the other hand, delineates the upper and lower, "wet" and "dry" bounds of the STR/VI scatterplot programmatically, through the following approach. Sentinel-2 images are acquired using the ``CDSE`` package [@karaman_cdse_2023], clipped to the study area, for the user-specified time range. The Application Programming Interface (API) request sent to the Copernicus DataSpace Ecosystem[^1] (CDSE) prepares both VI and STR indices. All pixel values for both indices, and all images along the time series are collected into a table and plotted as a scatterplot. The VI axis of the scatterplot is divided, programmatically, into a series of small intervals, and a subset of the STR values, within that narrow interval of VI is extracted. Outlier STR values are removed based on the accepted 1.5 times Inter Quartile Range (IQR) method. Then, among the remaining values, the top and bottom 2% quartiles of these STR values are found for each interval. The upper and lower quartile values are paired with the VI values for each interval, thus collecting points along both the "wet" and "dry trapezoid edges. Each of these two sets typically consists of 50 to 100 points. This approach offers a mathematically robust and repeatable implementation of the OPTRAM model.

One of three possible equations is fitted to each of these "wet" and "dry" sets of trapezoid edges (Figure \autoref{fig:diag}. A linear Ordinary Least Squares (OLS) regression line is fitted to each set of points in the most straightforward case. The intercept and slope of these lines give the coefficients for calculating soil water content. Two additional fitted options are implemented in ``rOPTRAM``: exponential and second-order polynomial. For OLS-fitted curves, two coefficients are derived for each line, the slope and intercept. Similarly, the exponential fitted curve requires two coefficients, the intercept and the multiplier of VI in the exponential term. A polynomial fit, on the other hand, consists of 3 coefficients, the intercept, and the coefficients for the first-order and second-order terms. 

![OPTRAM schematic diagram.\label{fig:diag}](GIS/OPTRAM_diagram.jpg)

[^1]: https://dataspace.copernicus.eu/

### Linear regression fit of trapezoid edges

$$  STR_{dry} = i_{dry} + s_{dry} \cdot VI $$

$$ STR_{wet} = i_{wet} + s_{wet} \cdot VI $$

where:
$i_{wet}, i_{dry}$ are the regression line intercepts, and
$s_{wet}, s_{dry}$ are the slopes respectively.

Then soil moisture can be derived from:

$$  W = \frac{STR - STR_{dry}}{STR_{wet} - STR_{dry}} $$

### Exponential curve fit of trapzoid edges

Exponential fitted curves for the "wet" and "dry" edges can be expressed as:

$$  STR_{dry} = i_{dry} \cdot exp(s_{dry} \cdot VI) $$

$$  STR_{wet} = i_{wet} \cdot exp(s_{wet} \cdot VI)  $$

and in this case, soil moisture is derived as:

$$  W = \frac{STR - (i_{dry} \cdot exp(s_{dry} \cdot VI))}{(i_{wet} \cdot exp(s_{wet} \cdot VI)) - (i_{dry} \cdot exp(s_{dry} \cdot VI))} $$

 
### Second-order polynomial fit of trapezoid edges

Polynomial fitted regression lines for the "wet" and "dry" edges can be expressed as:

$$  STR_{dry} = \alpha_{dry} + \beta1_{dry} \cdot VI + \beta2_{dry} \cdot VI^2 $$

$$  STR_{wet} = \alpha_{wet} + \beta1_{wet} \cdot VI + \beta2_{wet} \cdot VI^2 $$


and in this case, soil moisture is derived as:

$$ W = \frac{STR - (\alpha_{dry} + \beta1_{dry} \cdot VI + \beta2_{dry} \cdot VI^2)}{(\alpha_{wet} + \beta1_{wet} \cdot VI + \beta2_{wet} \cdot VI^2) - (\alpha_{dry} + \beta1_{dry} \cdot VI + \beta2_{dry} \cdot VI^2)} $$

# Acknowledgements

The authors wish to thank the two reviewers, Lauren O'Brien and Harry Eslick, whose comments substantially improved the code in this package for submission to ROpenSci.

# References

