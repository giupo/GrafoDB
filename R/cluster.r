#' @import rcf
NULL

if(is.windows()) {
  # Cluster Windows I hate you
  options(GCLUSTER=FALSE)
}

#' initializes a cluster for this package.
#'
#' @name initCluster
#' @usage initCluster()
#' @rdname initCluster
#' @usage initCluster(ncores)
#' @param ncores number of cores for this cluster, if `NULL`
#'               uses `parallel::detectCores`
#' @param ... other params passed to cluster constructor
#' @return an instance of a Cluster (as of `parallel` package).
#'         `NULL` if on Windows (it hangs!)
#' @import parallel doParallel
#' @export

initCluster <- function(ncores=NULL, ...) {
  if(getOption("SLAVE", FALSE)) {
    return(invisible(NULL))
  }

  if(!getOption("GCLUSTER", TRUE)) {
    return(invisible(NULL))
  }
  
  cl <- tryCatch(
    parallel:::defaultCluster(),
    error = function(cond) {
      NULL
    })
  
  if(is.null(cl)) {    
    if(is.null(ncores)) {
      ncores <- as.integer(detectCores())
    }
    
    cl <- if(is.windows()) {
      ## on windows we still have issues on this.
      ## when creating 2 cluster, it loops and hogs the machine
      makePSOCKcluster(ncores, ...)
    } else {
      makeForkCluster(ncores, outfile="~/.GrafoDB/cluster.log", ...)
      ## makePSOCKcluster(ncores, outfile="~/.GrafoDB/cluster.log", ...)
    }
  }
  
  if(is.null(cl)) {
    ## proprio non ce la faccio, ritorno NULL
    return(invisible(NULL))
  }
  clusterEvalQ(cl, options(SLAVE=TRUE))
  clusterEvalQ(cl, .libPaths("/home/group/main/894smf/cfin/library"))  
  clusterExport(
    cl, c(
      "tsRead",
      "tsWrite",
      "tempdir",
      "workDir",
      "whoami",
      "flypwd"),
    envir=environment())
  
  clusterEvalQ(cl, "require(rcf)")    
  clusterEvalQ(cl, "require(bimets)")
  clusterEvalQ(cl, "require(grafo)")
  clusterEvalQ(cl, "require(stringr)")
  clusterEvalQ(cl, "require(foreach)")
  clusterEvalQ(cl, "require(digest)")
  clusterEvalQ(cl, "require(plyr)")
  #clusterExport(cl, "pgConnect", envir=environment())
  #clusterExport(cl, "dbDisconnect", envir=environment())
  # connectCluster(cl)
  setDefaultCluster(cl)
  registerDoParallel(cl)
  invisible(cl)
}


connectCluster <- function(cl = NULL) {
  if(is.null(cl)) {
    cl <- initCluster()
  }
  clusterEvalQ(cl, {
    require(RPostgreSQL)
    pgConnect()
  })
}

disconnectCluster <- function(cl = NULL) {
  if(is.null(cl)) {
    cl <- initCluster()
  }  
  clusterEvalQ(cl, {
    require(RPostgreSQL)
    con <- getOption("pgConnect", NULL)
    if(!is.null(con)) {
      RPostgreSQL::dbDisconnect(con)
    }
  })
}
