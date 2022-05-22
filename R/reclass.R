#' @title Re-class the target map
#' @description re-class the base map to the target map
#' @param input (SpatRaster) an input terra raster to resample
#' @param output (SpatRaster) the output grid to resample to
#' @param nthread (integer) the number of thread to use for parallel. If `NULL`,
#' the all available cores in the machine will be used. Default to `NULL`.
#' @param verbose (logical) option to print out info for debugging.
#' @importFrom terra resample values<- freq ncell values
#' aggregate makeTiles free_RAM terraOptions zonal
#' @importFrom parallel mclapply detectCores
#' @importFrom collapse BY GRP fsubset
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

reclass <- function(input, output, nthread = NULL, verbose = FALSE){
  # Keep terra silent
  terraOptions(progress = 0)

  # Message
  if (verbose) message(sprintf('Start reclass - %s.', Sys.time()))

  # Check inputs
  checkmate::assert_class(input, 'SpatRaster', null.ok = FALSE)
  checkmate::assert_class(output, 'SpatRaster', null.ok = FALSE)
  checkmate::assert_int(nthread, null.ok = TRUE)
  checkmate::assert_logical(verbose)

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
  }; rm(n_left, inds, fname, num_vals_output)

  # Sort and remove classes with 0 needed cells
  count <- count[count$output_count != 0, ]
  count <- count[order(count$output_count), ]

  # Message
  if (verbose) message(sprintf('Summarize cells - %s.', Sys.time()))

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

  # Can work but slow
  opt <- utils::getFromNamespace("spatOptions", "terra")()
  opt$ncopies <- 1
  mem_need <- (input@ptr$mem_needs(opt)[1] * 2) * 3 / (1024^3 / 10)
  mem_avail <- input@ptr$mem_needs(opt)[3] *
    input@ptr$mem_needs(opt)[2] / (1024^3 / 8)
  rm(opt)

  # Calculate how many tiles to make
  num_tiles <- mem_need / mem_avail

  if (num_tiles < 0.5) {
    # Message
    if (verbose) message(sprintf('Take whole image to process - %s.',
                                 Sys.time()))

    # Overlay the base and target map
    zones <- output; values(zones) <- 1:ncell(zones)
    zones <- resample(zones, input, method = 'near',
                      wopt = list(gdal=c("COMPRESS=LZW")))
    cn <- zonal(input, zones, fun = function(x) list(x))
    names(cn) <- c('id', 'input')
    rm(zones)
    cn <- cn[cn$id == 0, ]

    # Group by Output object and cell value, count, and determine percentage
    areal_per <- do.call(
      rbind, lapply(1:nrow(cn), function(n) {
          props <- get_proportion(cn[[n, 'input']], count$value)
          dt <- data.frame(t(props))
          names(dt) <- count$value
          dt$id <- n
          dt[, c('id', count$value)]
      })); rm(cn)

    # Re-class the target pixels
    ## Fill all cells with 100% of one class with that class
    inds <- apply(areal_per[, -1] == 10000, 1, match, x = TRUE)
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
        !rowSums(is.na(areal_per[, -c(1, ncol(areal_per))])) ==
        ncol(areal_per[, -c(1, ncol(areal_per))]),
      as.numeric(colnames(areal_per[, -c(1, ncol(areal_per))])[
        unlist(sapply(apply(areal_per[, -c(1, ncol(areal_per))], 1, which.max),
                      function(x) unname(x[1])))]),
      as.numeric(areal_per$cat))

    # Clean up
    rm(num_to_fill, per_this_class, inds_to_fill)

    matrix(areal_per$cat, nrow = nrow(output),
           ncol = ncol(output), byrow = TRUE)

  } else {
    # Message
    if (verbose) message(sprintf('Use chunks for large image - %s.',
                                 Sys.time()))

    num_tiles <- ceiling(num_tiles)
    num_factor <- floor(sqrt(ncell(output) / num_tiles))
    # The map would be too huge, just give it a try
    if (num_factor <= 1) num_factor <- 2

    # Cut images to chunks
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

    # Message
    if (verbose) message(sprintf('Cut image to chunks - %s.', Sys.time()))

    # Calculate
    areal_per <- lapply(1:length(output_tiles), function(n) {
        # Message
        if (verbose) message(sprintf('Chunk %s of %s - %s.',
                                     n, length(output_tiles), Sys.time()))
        
        # Overlay the base and target map
        zones <- rast(output_tiles[n])
        n_row <- nrow(zones); n_col <- ncol(zones)
        values(zones) <- 1:ncell(zones)
        chunk <- rast(input_tiles[n])
        fname <- tempfile(fileext = '.tif')
        zones <- resample(zones, chunk, method = 'near',
                          filename = fname, 
                          overwrite = TRUE,
                          datatype = "INT4U",
                          gdal = c("COMPRESS=LZW"))
        vals <- values(c(chunk, zones))
        rm(zones, chunk); file.remove(fname); free_RAM()
        colnames(vals) <- c('cats', 'groups')
        if (any(is.na(vals[, 'groups']))){
            vals <- fsubset(vals, !is.na(vals[, 'groups']))}
        vals <- fsubset(vals, vals[, 'groups'] != 0)
        # areal_per_blk <- BY(vals[, 'cats'], GRP(vals[, 'groups']), 
        #                     FUN = function(x) {
        #                         props <- get_proportion(x, count$value)
        #                         dt <- data.frame(t(props))
        #                         names(dt) <- count$value
        #                         dt
        #                     }, expand.wide = TRUE)
        num_split <- ceiling(n_row * n_col / nthread)
        fnames <- file.path(tempdir(), sprintf('tmp_mat_%s.rda', 1:nthread))
        n_to_select <- num_split
        for (i in 1:nthread) {
            if (i == nthread) {
                vals_temp <- vals
                save(vals_temp, file = fnames[i])
            } else {
                vals_temp <- fsubset(vals, vals[, 'groups'] <= n_to_select)
                vals <- fsubset(vals, vals[, 'groups'] > n_to_select)
                save(vals_temp, file = fnames[i])
            }
            n_to_select <- n_to_select + num_split
            rm(vals_temp)
        }; rm(vals); gc()
        
        areal_per_blk <- do.call(rbind, mclapply(fnames, function(fname) {
            load(fname)
            BY(vals_temp[, 'cats'], GRP(vals_temp[, 'groups']), 
               FUN = function(x) {
                   props <- get_proportion(x, count$value)
                   dt <- data.frame(t(props))
                   names(dt) <- count$value
                   dt
               }, expand.wide = TRUE)
        }, mc.cores = nthread))
        file.remove(fnames); gc()
        
        # Return
        inds <- apply(areal_per_blk == 10000, 1, match, x = TRUE)
        areal_per_blk$id <- 1:c(n_row * n_col)
        areal_per_blk <- areal_per_blk[, c('id', count$value)]
        areal_per_blk$cat <- count$value[inds]; rm(inds)
        list(areal_per_blk, c(n_row, n_col))
    })

    # Get the ncell of each tile and rbind all pixels together
    ncell_tiles <- sapply(areal_per, function(x) nrow(x[[1]]))
    groups <- seq_along(ncell_tiles)
    groups <- unlist(lapply(seq_along(ncell_tiles), function(n) {
      rep(groups[n], ncell_tiles[n])}))
    n_row_cols <- mclapply(areal_per, function(x) x[[2]],
                           mc.cores = min(length(areal_per), nthread))
    areal_per <- do.call(
      rbind, mclapply(areal_per, function(x) x[[1]],
                      mc.cores = min(length(areal_per), nthread)))
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
        !rowSums(is.na(areal_per[, -c(1, ncol(areal_per))])) ==
        ncol(areal_per[, -c(1, ncol(areal_per))]),
      as.numeric(colnames(areal_per[, -c(1, ncol(areal_per))])[
        unlist(sapply(apply(areal_per[, -c(1, ncol(areal_per))], 1, which.max),
                      function(x) unname(x[1])))]),
      as.numeric(areal_per$cat))

    # Clean up
    rm(num_to_fill, per_this_class, inds_to_fill)
    free_RAM(); gc()

    # Message
    if (verbose) message(sprintf("Reassign values - %s.", Sys.time()))

    # Reshape the result to tiles
    target_cats <- mclapply(seq_along(ids_in_group), function(n) {
      inds <- ids_in_group[[n]]
      n_row <- n_row_cols[[n]][1]
      n_col <- n_row_cols[[n]][2]
      matrix(areal_per$cat[inds], nrow = n_row, ncol = n_col, byrow = TRUE)
    }, mc.cores = min(length(ids_in_group), nthread))
    rm(areal_per, ids_in_group, n_row_cols)
    unlink(temp_dir, recursive = TRUE)

    # Message
    if (verbose) message(sprintf('Reshape the matrix - %s.', Sys.time()))

    # Mosaic the matrices
    ids_to_rbind <- 1:length(target_cats) %% dims_out[2]
    do.call(cbind, mclapply(unique(ids_to_rbind), function(n) {
      do.call(rbind, target_cats[which(ids_to_rbind == n)])
    }, mc.cores = min(length(unique(ids_to_rbind)), nthread)))
  }
}

# end reclass
