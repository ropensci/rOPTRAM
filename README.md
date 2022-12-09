
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rOPTRAM

<!-- badges: start -->
<!-- badges: end -->

`rOPTRAM` implements the Optical Trapezoid Model to derive soil moisture
from satellite images. See Sadeghi et al. (2017)

## Installation

`rOPTRAM` resides on gitlab. You can install the development version of
rOPTRAM like so:

``` r
# Install remotes package
if (! require("remote")) {
  install.packages("remotes")
}
remotes::install_gitlab("https://gitlab.com/rsl-bidr/roptram.git")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# library(rOPTRAM)
## basic example code
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-sadeghi_optical_2017" class="csl-entry">

Sadeghi, Morteza, Ebrahim Babaeian, Markus Tuller, and Scott B. Jones.
2017. “The Optical Trapezoid Model: A Novel Approach to Remote Sensing
of Soil Moisture Applied to Sentinel-2 and Landsat-8 Observations.”
*Remote Sensing of Environment* 198 (September): 52–68.
<https://doi.org/10.1016/j.rse.2017.05.041>.

</div>

</div>
