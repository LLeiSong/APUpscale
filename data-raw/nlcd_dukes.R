## code to prepare `nlcd_dukes` dataset goes here
library(terra)
library(dplyr)
library(sf)

# Download NLCD land cover map of 2019
url <- file.path('https://s3-us-west-2.amazonaws.com',
                 'mrlc/nlcd_2019_land_cover_l48_20210604.zip')
fname <- tempfile()
download.file(url, fname)

# Unzip huge files reliably
system2("unzip", args = c("-o",
                          fname,
                          "-d",
                          file.path(tempdir(), 'nlcd_2019')))
nlcd <- rast(list.files(file.path(tempdir(), 'nlcd_2019'),
                        pattern = '.img', full.names = TRUE))

# Define an Area of Interest and transform to NLCD CRS
AOI <- USAboundaries::us_counties(resolution = "low", states = 'MA') %>%
  dplyr::select(-state_name) %>%
  dplyr::filter(name == 'Dukes') %>%
  st_transform(crs(nlcd)) %>% vect()
nlcd_dukes <- mask(crop(nlcd, AOI, snap = 'out'), AOI)
coltab(nlcd_dukes) <- coltab(nlcd)
levels(nlcd_dukes) <- NULL
writeRaster(nlcd_dukes,
            'inst/extdata/nlcd_dukes.tif',
            datatype = 'INT1U',
            gdal = c("COMPRESS=LZW"))

# Clean up
file.remove(fname)
unlink(file.path(tempdir(), 'nlcd_2019'), recursive = TRUE)
# usethis::use_data(nlcd_dukes, overwrite = TRUE)
