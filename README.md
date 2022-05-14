# APUpscale
<!-- badges: start -->
[![R-CMD-check](https://github.com/LLeiSong/APUpscale/workflows/R-CMD-check/badge.svg)](https://github.com/LLeiSong/APUpscale/actions)
<!-- badges: end -->

## Overview
The package provides a workable way to upscale huge categorical map using area preserving method. It can ensure both the compositional information and landscape characteristics.

For a small map, it might be less efficient. And you can use package [resample](https://github.com/mikejohnson51/resample).

## Installation
You can install the development version from GitHub with:

```
# install.packages("devtools")
devtools::install_github("LLeiSong/APUpscale")
```

## Usage

```{r}
library(APUpscale)
data("nlcd_worcester")
lc_coarse <- upscale(nlcd_worcester, cellsize = 1000)
coltab(lc_coarse) <- coltab(nlcd_worcester)
plot(lc_coarse)
```

## References

Johnson, J. Michael, and Keith C. Clarke. "An area preserving method for improved categorical raster resampling." Cartography and Geographic Information Science 48.4 (2021): 292-304.
