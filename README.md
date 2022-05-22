# APUpscale
<!-- badges: start -->
[![R-CMD-check](https://github.com/LLeiSong/APUpscale/workflows/R-CMD-check/badge.svg)](https://github.com/LLeiSong/APUpscale/actions)
<!-- badges: end -->

## Overview
The package provides a workable way to upscale huge categorical map using area preserving method. It can ensure both the compositional information and landscape characteristics. It does the calculation in two ways:

- If the base map is small enough in terms of machine RAM, it relies on `terra::zonal` 'to summarise cells.
- If the base map is way large than machine RAM, it will cut the base map into chunks. And for each chunk, it converts rasters to matrix and do grouping aggregation taking advantage of both base R and package `collapse`. In order to avoid RAM crash, it uses a conservative way to deal with RAM.

*NOTE:* For a moderate size map, it might be less efficient. And if the base map is large and the upscale factor is small (e.g. upscaling 4m to 30m), it might be very slow but still workable.

## Installation
You can install the development version from GitHub with:

```
# install.packages("devtools")
devtools::install_github("LLeiSong/APUpscale")
```

## Usage

```{r}
library(APUpscale)
library(terra)
nlcd <- rast(system.file('extdata/nlcd_dukes.tif', package = "APUpscale"))
lc_coarse <- upscale(nlcd, cellsize = 1000, nthread = 2)
coltab(lc_coarse) <- coltab(nlcd)
plot(lc_coarse)
```

## References

1. Johnson, J. Michael, and Keith C. Clarke. "An area preserving method for improved categorical raster resampling." Cartography and Geographic Information Science 48.4 (2021): 292-304.

2. [mikejohnson51/resample](https://github.com/mikejohnson51/resample)
