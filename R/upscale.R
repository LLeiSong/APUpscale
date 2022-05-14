#' @title upscale
#' @description upscale an input raster to an output grid
#' using an area preserving method
#' @param input (SpatRaster) a terra raster with fine resolution
#' @param cellsize (numeric) cell resolution in units of input CRS of
#' output target grid.
#' @param no_data (integer) a value to be treated as NO_DATA. Default to NA.
#' @param nthread (integer) the number of thread to use for parallel.
#' Default to `parallel::detectCores()`.
#' @importFrom terra rast writeRaster
#' @importFrom parallel detectCores
#'
#' @return (SpatRaster) a resampled terra raster
#' @export

upscale <- function(input, cellsize,
                    no_data = NA,
                    nthread = detectCores()){
  # Check inputs
  checkmate::assert_class(input, 'SpatRaster', null.ok = FALSE)
  checkmate::assert_number(cellsize,
                           lower = max(res(input)),
                           upper = min(ext(input)[2] - ext(input)[1],
                                       ext(input)[4] - ext(input)[3]))
  if (any(cellsize %% res(input) != 0)) {
    warning(sprintf('%s is not a integral factor of original resolution.',
                    cellsize))}
  checkmate::assert_number(no_data, na.ok = TRUE)

  # Make the output raster template
  output <- rast(ext(input), crs = crs(input), resolution = cellsize)

  # Set no_data to NA for convenience
  if (!is.na(no_data)) {
    input <- classify(input, cbind(no_data, NA),
                      filetype = 'GTiff',
                      gdal = c("COMPRESS=LZW"))}

  # Summarize the output cells
  target_map <- reclass(input,
                        output = output,
                        nthread = nthread)

  # Set NA back to no_data
  if (!is.na(no_data)) {
    target_map <- classify(target_map, cbind(NA, no_data),
                      filetype = 'GTiff',
                      gdal = c("COMPRESS=LZW"))}

  # Return
  target_map
}

# end upscale
