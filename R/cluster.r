#' @import rcf
NULL

if(is.windows()) {
  # Cluster Windows I hate you
  options(GCLUSTER=FALSE)
}

#' initializes a cluster for this package.
#'
#' @name .initDefaultCluster
#' @usage .initDefaultCluster()
#' @rdname initDefaultCluster
#' @usage .initDefaultCluster(ncores)
#' @param ncores number of cores for this cluster, if `NULL`
#'               uses `parallel::detectCores`
#' @param ... other params passed to cluster constructor
#' @return an instance of a Cluster (as of `parallel` package).
#'         `NULL` if on Windows (it hangs!)
#' @import parallel doParallel
#' @export

initDefaultCluster <- function(ncores=NULL, ...) {
  if(!getOption("GCLUSTER", TRUE)) {
    return(invisible(NULL))
  }

  if(is.null(ncores)) {
    ncores <- detectCores() * 2
  }

  cl <- tryCatch(
    parallel:::defaultCluster(),
    error = function(cond) {
      NULL
    })
  
  if(!is.null(cl)) {
    return(cl)
  }

  cl <- if(is.windows()) {
    ## on windows we still have issues on this.
    ## when creating 2 cluster, it loops and hogs the machine
    makePSOCKcluster(ncores, ...)
  } else {
    makeForkCluster(ncores, ...)
    #hosts_ids <- c("006", "007") #, "008", "010", "011", "018", "019", "038", "039", "040", "041")
    #hostnames <- paste0("osiride-lp-", hosts_ids)
    #makePSOCKcluster(master = Sys.info()[["nodename"]], names = hostnames, nnodes=detectCores() * length(hostnames))
  }

  if(is.null(cl)) {
    return(invisible(NULL))
  }
  
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
  clusterEvalQ(cl, "require(digest)")
  clusterEvalQ(cl, "require(RPostgreSQL)")
  clusterEvalQ(cl, "require(plyr)")
  clusterExport(cl, "pgConnect")
  clusterExport(cl, "dbDisconnect")
  clusterEvalQ(cl, {
    require(RPostgreSQL)
    pgConnect()
  })
  setDefaultCluster(cl)
  registerDoParallel(cl)
  invisible(cl)
}
