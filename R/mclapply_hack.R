############## Original code ##############
## mclapply.hack.R
## Nathan VanHoudnos
## nathanvan AT northwestern FULL STOP edu
## July 14, 2014
###########################################

#' @title Windows version of parallel apply function to use
#' @description A script to implement a hackish version of parallel:mclapply()
#' on Windows machines.
#' @param ... Any inputs works for `makeCluster`.
#' @importFrom parallel makeCluster clusterExport parLapply stopCluster
#' @export


## Define the hack
mclapply_hack <- function(...) {
  ## Create a cluster
  size.of.list <- length(list(...)[[1]])
  cl <- makeCluster(min(size.of.list, detectCores()))

  ## Find out the names of the loaded packages
  loaded.package.names <- c(
    ## Base packages
    sessionInfo()$basePkgs,
    ## Additional packages
    names(sessionInfo()$otherPkgs)
  )

  tryCatch(
    {

      ## Copy over all of the objects within scope to
      ## all clusters.
      this.env <- environment()
      while (identical(this.env, globalenv()) == FALSE) {
        clusterExport(cl,
          ls(all.names = TRUE, env = this.env),
          envir = this.env
        )
        this.env <- parent.env(environment())
      }
      clusterExport(cl,
        ls(all.names = TRUE, env = globalenv()),
        envir = globalenv()
      )

      ## Load the libraries on all the clusters
      ## N.B. length(cl) returns the number of clusters
      parLapply(cl, 1:length(cl), function(xx) {
        lapply(loaded.package.names, function(yy) {
          require(yy, character.only = TRUE)
        })
      })

      ## Run the lapply in parallel
      return(parLapply(cl, ...))
    },
    finally = {
      ## Stop the cluster
      stopCluster(cl)
    }
  )
}

## end mclapply.hack
