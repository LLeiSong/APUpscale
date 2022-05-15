#' @title Re-class the target map
#' @description re-class the base map to the target map
#' @param input (SpatRaster) an input terra raster to resample
#' @param output (SpatRaster) the output grid to resample to
#' @param nthread (integer) the number of thread to use for parallel.
#' Default to \code{\link{detectCores}}.
#' @importFrom terra resample values<- freq ncell values aggregate makeTiles free_RAM zonal
#' @importFrom parallel mclapply detectCores
#' @importFrom stats na.omit
#' @importFrom utils getFromNamespace
#' @return (matrix) the categorical matrix of target terra raster
#' @export
#' @examples
#' library(APUpscale)
#' library(terra)
#' nlcd <- rast(system.file('extdata/nlcd_dukes.tif', package = "APUpscale"))
#' output <- rast(ext(nlcd), crs = crs(nlcd), resolution = 1000, vals = NA)
#' target_vals <- reclass(nlcd, output, nthread = 2)
#' values(output) <- target_vals
#' coltab(output) <- coltab(nlcd)
#' plot(output)

reclass <- function(input, output, nthread = NULL){

  # Check inputs
  checkmate::assert_class(input, 'SpatRaster', null.ok = FALSE)
  checkmate::assert_class(output, 'SpatRaster', null.ok = FALSE)
  checkmate::assert_int(nthread, null.ok = TRUE)

  # Adjust number of threads to use
  if (is.null(nthread)) nthread <- detectCores()
  nthread <- min(nthread, detectCores())

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
                                   filename = fname, overwrite = TRUE,
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
      as.integer(round(freqs$freq / length(x) * 10000))
    }
  }

  ## Do parallel according to operation platform
  mclapply <- switch( Sys.info()[['sysname']],
                      Windows = {mclapply_hack},
                      Linux   = {mclapply},
                      Darwin  = {mclapply})

  # Check memory
  opt <- utils::getFromNamespace("spatOptions", "terra")()
  opt$ncopies <- 1
  mem_need <- (input@ptr$mem_needs(opt)[1] +
                 output@ptr$mem_needs(opt)[1]) / (1024^3 / 8)
  mem_avail <- input@ptr$mem_needs(opt)[3] *
    input@ptr$mem_needs(opt)[2] / (1024^3 / 8)
  rm(opt)

  # Calculate how many tiles to make
  num_tiles <- ceiling(mem_need / floor(mem_avail / nthread))
  num_factor <- floor(sqrt(ncell(output) / num_tiles))
  # The map would be too huge
  if (num_factor <= 0) num_factor <- 1

  # Split raster to tiles
  temp_dir <- file.path(tempdir(), 'tiles')
  if (!dir.exists(temp_dir)) dir.create(temp_dir)
  template <- aggregate(output, fact = num_factor)
  dims_out <- c(nrow(template), ncol(template))
  output_tiles <- makeTiles(
    output, template,
    filename = file.path(temp_dir, 'output_.tif'))
  input_tiles <- makeTiles(
    input, template,
    filename = file.path(temp_dir, 'input_.tif'))
  rm(num_tiles, num_factor, mem_need, mem_avail)
  rm(input, output, template); free_RAM(); gc()

  # Calculate
  areal_per <- mclapply(1:length(output_tiles), function(n) {
    # # Overlay the base and target map
    base_map <- rast(input_tiles[n])
    zones <- rast(output_tiles[n])
    n_row <- nrow(zones); n_col <- ncol(zones)
    values(zones) <- 1:ncell(zones)
    fname <- tempfile(fileext = '.tif')
    zones <- resample(zones, base_map, method = 'near',
                      filename = fname,
                      wopt = list(gdal=c("COMPRESS=LZW")))
    cn <- zonal(base_map, zones, fun = function(x) list(x))
    names(cn) <- c('id', 'input')
    file.remove(fname); rm(zones, fname, base_map)
    free_RAM(); gc()

    areal_per_blk <- do.call(rbind, mclapply(1:nrow(cn), function(n) {
      props <- get_proportion(cn[[n, 'input']], count$value)
      dt <- data.frame(t(props))
      names(dt) <- count$value
      dt$id <- cn[n, ]$id
      dt[, c('id', count$value)]
    }, mc.cores = nthread))

    inds <- apply(areal_per_blk[, -1] == 10000, 1, match, x = TRUE)
    areal_per_blk$cat <- count$value[inds]; rm(inds)
    list(areal_per_blk, c(n_row, n_col))
  }, mc.cores = min(length(output_tiles), nthread))

  # Get the ncell of each tile and rbind all pixels together
  ncell_tiles <- sapply(areal_per, function(x) nrow(x[[1]]))
  groups <- letters[seq_along(ncell_tiles)]
  groups <- unlist(lapply(seq_along(ncell_tiles), function(n) {
    rep(groups[n], ncell_tiles[n])}))
  n_row_cols <- lapply(areal_per, function(x) x[[2]])
  areal_per <- do.call(
    rbind, lapply(areal_per, function(x) x[[1]]))
  ids_in_group <- split(1:nrow(areal_per), groups)
  rm(ncell_tiles, groups)

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
    per_this_class[per_this_class <= 1000] <- NA

    # Pick up num_to_fill of pixels with the most coverage to assign class
    num_to_fill <- min(num_to_fill, sum(!is.na(per_this_class)))
    if (num_to_fill > 0) {
      inds_to_fill <- order(per_this_class, decreasing = T)[1:num_to_fill]
      areal_per$cat[inds_to_fill]= as.integer(class_id)}
  }

  # If a cell is unassigned, implement a majority rule
  areal_per$cat <- ifelse(
    is.na(areal_per$cat) &
      !is.na(rowSums(areal_per[, -c(1, ncol(areal_per))])),
         as.numeric(colnames(areal_per[, -c(1, ncol(areal_per))])[
           apply(areal_per[, -c(1, ncol(areal_per))], 1, which.max)]),
         as.numeric(areal_per$cat))

  # Clean up
  rm(num_to_fill, per_this_class, inds_to_fill)
  free_RAM(); gc()

  # Reshape the result to tiles
  target_cats <- mclapply(seq_along(ids_in_group), function(n) {
    inds <- ids_in_group[[n]]
    n_row <- n_row_cols[[n]][1]
    n_col <- n_row_cols[[n]][2]
    matrix(areal_per$cat[inds], nrow = n_row, ncol = n_col, byrow = TRUE)
  }, mc.cores = min(length(ids_in_group), nthread))
  rm(areal_per, ids_in_group, n_row_cols)
  unlink(temp_dir, recursive = TRUE)

  # Mosaic the matrices
  ids_to_rbind <- 1:length(target_cats) %% dims_out[2]
  do.call(cbind, mclapply(unique(ids_to_rbind), function(n) {
    do.call(rbind, target_cats[which(ids_to_rbind == n)])
  }, mc.cores = min(length(unique(ids_to_rbind)), nthread)))
}

# end reclass
