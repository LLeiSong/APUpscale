############## Original code ##############
## mclapply.hack.R
## Nathan VanHoudnos
## nathanvan AT northwestern FULL STOP edu
## July 14, 2014
## Modified by Nathan VanHoudnos
## https://github.com/nathanvan/parallelsugar/
## blob/master/R/mclapply_socket.R
###########################################

#' @title Windows version of parallel apply function to use
#' @description A script to implement a hackish version of parallel:mclapply()
#' on Windows machines.
#' @param X (vector (atomic or list)) the vector to loop on.
#' @param FUN the function to be applied to each element of `X` in parallel to.
#' @param ... Any inputs works for `makeCluster`.
#' @importFrom parallel makeCluster clusterExport parLapply stopCluster
#' @importFrom utils sessionInfo
#' @export
#' @examples
#' library(APUpscale)
#' library(parallel)
#' mclapply <- switch( Sys.info()[['sysname']],
#'                     Windows = {mclapply_hack},
#'                     Linux   = {mclapply},
#'                     Darwin  = {mclapply})


## Define the hack
mclapply_hack <- function(X, FUN, ...) {
  ## Create a cluster
  if (!"mc.cores" %in% list(...)) {
    mc.cores <- min(length(X), detectCores())
  } else {
    if (is.null(mc.cores)) {
      mc.cores <- min(length(X), detectCores())
    }
  }
  cl <- makeCluster(mc.cores)

  tryCatch({
    ## Find out the names of the loaded packages
    loaded.package.names <- c(
      ## Base packages
      sessionInfo()$basePkgs,
      ## Additional packages
      names( sessionInfo()$otherPkgs))

    ### Ship it to the clusters
    clusterExport(cl, 'loaded.package.names',
                  envir = environment())

    ## Load the libraries on all the clusters
    ## N.B. length(cl) returns the number of clusters
    parLapply(cl, 1:length(cl), function(xx){
      lapply(loaded.package.names, function(yy) {
        require(yy, character.only = TRUE)})
    })


    .clusterExport_function(cl, FUN)

    ## Run the lapply in parallel, with a special case for the ... arguments
    if(length(list(...)) == 0) {
      return(parLapply( cl = cl, X = X, fun = FUN))
    } else {
      return(parLapply( cl = cl, X = X, fun = FUN, ...))
    }
  }, finally = {
    ## Stop the cluster
    parallel::stopCluster(cl)
  })
}


#' @title Internal function to enclose environment
#' @description A script called by mclapply_hack to enclose environment.
#' @param cl (vector (atomic or list)) the vector to loop on.
#' @param FUN the function to be applied to each element of `X` in parallel to.
#' @importFrom parallel clusterExport

.clusterExport_function <- function(cl, FUN) {
  ## Written by Hadley Wickham, off the top of his head, when I asked him
  ##   for help at one of his Advanced R workshops.
  env <- environment(FUN)
  while(!identical(env, globalenv())) {
    env <- parent.env(env)
    clusterExport(cl, ls(all.names = TRUE, envir = env), envir = env)
  }
  clusterExport(cl, ls(all.names = TRUE, envir = env), envir = env)
  ## // End Hadley Wickham
}

## end mclapply_hack
