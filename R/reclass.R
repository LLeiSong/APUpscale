#' @title Re-class the target map
#' @description re-class the base map to the target map
#' @param input (SpatRaster) an input terra raster to resample
#' @param output (SpatRaster) the output grid to resample to
#' @param nthread (integer) the number of thread to use for parallel.
#' Default to `parallel::detectCores()`.
#' @importFrom terra resample values
#' @importFrom raster raster crop
#' @importFrom parallel mclapply detectCores
#' @return (SpatRaster) the target terra raster
#' @export

reclass <- function(input, output, nthread = detectCores()){

  # Remove levels if any
  levels(input) <- NULL

  # Identify the corresponding number of cells in the output
  ## Step 1: calculate the percentage of each class in base map
  ## Step 2: calculate the rough number of pixels of each class in target map
  ## Step 3: adjust the remaining pixels
  count <- data.frame(freq(input)[, c('value', 'count')])

  # Get expected number of NAs in output
  fname <- tempfile(fileext = '.tif')
  num_vals_output <- freq(resample(input, output, method = 'near',
                                   filename = fname,
                                   wopt = list(gdal=c("COMPRESS=LZW"))),
                          value = NA)[[1, 'count']]
  file.remove(fname)

  # Calculate the expected number of pixels in the target map
  count$count <- count$count / sum(count$count) *
    (ncell(output) - num_vals_output)

  # Floor the needed output values
  count$output_count <- floor(count$count)
  n_left <- ncell(output) - num_vals_output - sum(count$output_count)

  # In case there are more left pixels than classes
  while (n_left > 0) {
    inds <- order(count$count - count$output_count,
                  decreasing = TRUE)[1:n_left]
    count$output_count[inds] = count$output_count[inds] + 1
    n_left <- ncell(output) - num_vals_output - sum(count$output_count)
  }; rm(n_left, inds, fname)

  # Sort and remove classes with 0 needed cells
  count <- count[count$output_count != 0, ]
  count <- count[order(count$output_count), ]

  # Determine which input cells underlay each output object
  get_proportion <- function(x, classes) {
    if (all(is.na(x))){
      rep(NA, length(classes))
    } else {
      freqs <- data.frame(table(x))
      names(freqs) <- c('class', 'freq')
      freqs <- merge(data.frame(class = classes),
                     freqs, by = 'class', all = TRUE)
      freqs <- freqs[match(classes, freqs$class), ]
      as.integer(round(freqs$freq / length(x) * 1000))
    }
  }

  ###################### Terra way #####################
  # # Overlay the base and target map
  # zones <- output; values(zones) <- 1:ncell(zones)
  # fname <- tempfile(fileext = '.tif')
  # zones <- resample(zones, input, method = 'near',
  #                   filename = fname,
  #                   wopt = list(gdal=c("COMPRESS=LZW")))
  # cn <- zonal(input, zones, fun = function(x) list(x))
  # names(cn) <- c('id', 'input')
  # file.remove(fname); rm(zones, fname)

  ## Do parallel according to operation platform
  mclapply <- switch( Sys.info()[['sysname']],
                      Windows = {mclapply_hack},
                      Linux   = {mclapply},
                      Darwin  = {mclapply})

  # Convert to use raster and do paralleling calculation
  ## Workable for super large map
  input <- raster(input)
  output <- raster(output)
  n_rows <- nrow(output)
  n_cols <- ncol(output)
  n_cells <- ncell(output)

  # Group by Output object and cell value, count, and determine percentage
  areal_per <- do.call(
    rbind, lapply(1:n_rows, function(n_row) {
    do.call(rbind, mclapply(1:n_cols, function(n_col) {
      piece <- crop(input,
                    output[n_row, n_col, drop = FALSE],
                    snap = 'out', mask = TRUE)
      vals <- values(piece)
      props <- get_proportion(vals, count$value)
      dt <- data.frame(t(props))
      names(dt) <- count$value
      dt$id <- n_col + (n_row - 1) * n_cols
      dt[, c('id', count$value)]
    }, mc.cores = nthread))
  })); rm(n_rows, n_cols, n_cells)

  # Re-class the target pixels
  ## Fill all cells with 100% of one class with that class
  inds <- apply(areal_per[, -1] == 1000, 1, match, x = TRUE)
  areal_per$cat <- count$value[inds]; rm(inds)

  # Pick pixels in target map for classes
  for (class_id in count$value) {
    # Calculate the number of remaining pixels to assign
    num_to_fill <- count[count$value == class_id, 'output_count'] -
      sum(na.omit(areal_per$cat) == class_id)
    # In case more than output_count pixels have been assigned
    if (num_to_fill < 0) num_to_fill <- 0

    # Get the column of this class
    per_this_class <- areal_per[[as.character(class_id)]]
    # Remove pixels that have assigned class already
    per_this_class[which(!is.na(areal_per$cat))] <- NA
    # Remove pixels with less than 10% to be this class
    # to reduce calculation
    per_this_class[per_this_class <= 100] <- NA

    # Pick up num_to_fill of pixels with the most coverage to assign class
    num_to_fill <- min(num_to_fill, sum(!is.na(per_this_class)))
    inds_to_fill <- order(per_this_class, decreasing = T)[1:num_to_fill]
    areal_per$cat[inds_to_fill]= as.integer(class_id)
  }
  # Clean up
  rm(num_to_fill, per_this_class, inds_to_fill)

  # Clean up and return
  output <- rast(output)
  values(output) <- areal_per$cat

  output
}

# end reclass
