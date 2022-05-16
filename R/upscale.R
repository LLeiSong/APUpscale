#' @title upscale
#' @description upscale an input raster to an output grid
#' using an area preserving method
#' @param input (SpatRaster) a terra raster with fine resolution
#' @param cellsize (numeric) cell resolution in units of input CRS of
#' output target grid.
#' @param no_data (integer) a value to be treated as NO_DATA. Default to NA.
#' @param nthread (integer) the number of thread to use for parallel.
#' Default to `NULL`. It is recommended to use small ones to
#' take full advantage of `terra`, for instance 2, 4.
#' @param verbose (logical) print out info for debugging.
#' @importFrom terra rast writeRaster ext crs classify res values
#' @importFrom parallel detectCores
#'
#' @return (SpatRaster) a resampled terra raster
#' @export
#' @examples
#' library(APUpscale)
#' library(terra)
#' nlcd <- rast(system.file('extdata/nlcd_dukes.tif', package = "APUpscale"))
#' lc_coarse <- upscale(nlcd, cellsize = 1000, nthread = 2)
#' coltab(lc_coarse) <- coltab(nlcd)
#' plot(lc_coarse)

upscale <- function(input, cellsize,
                    no_data = NA,
                    nthread = NULL,
                    verbose = FALSE){
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
  checkmate::assert_int(nthread, null.ok = TRUE)
  checkmate::assert_logical(verbose)

  # Adjust number of threads to use
  if (is.null(nthread)) nthread <- detectCores()
  nthread <- min(nthread, detectCores())

  # Make the output raster template
  output <- rast(ext(input), crs = crs(input),
                 resolution = cellsize,
                 vals = NA)

  # Set no_data to NA for convenience
  if (!is.na(no_data)) {
    input <- classify(input, cbind(no_data, NA),
                      filetype = 'GTiff',
                      overwrite = TRUE,
                      gdal = c("COMPRESS=LZW"))
    # Message
    if (verbose) message(sprintf('Set no data to NA - %s.', Sys.time()))
    }


  # Summarize the output cells
  target_vals <- reclass(input,
                         output = output,
                         nthread = nthread,
                         verbose = verbose)
  values(output) <- target_vals

  # Message
  if (verbose) message(sprintf('Upscale the map - %s.', Sys.time()))

  # Set NA back to no_data
  if (!is.na(no_data)) {
    output <- classify(output, cbind(NA, no_data),
                      filetype = 'GTiff',
                      overwrite = TRUE,
                      gdal = c("COMPRESS=LZW"))
    # Message
    if (verbose) message(sprintf('Set NA back to no data - %s.', Sys.time()))
    }

  # Return
  output
}

# end upscale
