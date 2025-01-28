
rOPTRAM 0.3.0 (2024-08-15)
=========================

### NEW FEATURES

    * Adds option to filter out clouds, cloud shadows, water, using the Sentinel-2 SCM mask layer.
    * Implements check for existing STR and VI files to avoid downloading again and overwriting. 
    * Adds option to save list of images from CDSE::SearchCatalog after filtering for max cloud cover.
    * Adds option to set output resolution of downloaded images.


rOPTRAM 0.2.0 (2024-07-02)
=========================

### NEW FEATURES

  * Implements `optram_options()` function to allow setting many algorithm options that are applied throughout the model.
  * Implements four coloring options for the VI-STR scatter plot: colors by point density, colors by a categorical attribute column in the aoi polygon, colors by image date, uniform color with contour lines of point density, 
  * Adds tileid (Sentinel-2 tiles) to downloaded images. This allows to save images from adjacent tiles when an AOI extends beyond a single tile.
  * The `CDSE` function `GetArchiveImage()` has been deprecated. This version of `rOPTRAM` uses the new `GetImage()` function.
  
rOPTRAM 0.2.0 (2024-06-06)
=========================

FIRST RELEASE ON ROPENSCI



rOPTRAM 0.1.0 (2024-04-30)
=========================

INITIAL RESPONSE TO REVIEWS

### NEW FEATURES

  * Implements three curve fitting algorithms to determine OPTRAM trapezoid.

### MINOR IMPROVEMENTS

  * Removes unnecessary tests.
  * Requires user to input area of interest as \code{sf} object.
  * Plot of trapezoid returned as \code{ggplot2} plot to allow user to apply further tweaks.

### BUG FIXES

  * Fixes path to images in vignettes

### DOCUMENTATION FIXES

  * Fixes documentation regarding download with \code{CDSE} package
  * Edits in keeping with [r-lib guide](https://roxygen2.r-lib.org/articles/formatting.html).

