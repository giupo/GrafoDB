#' onLoad
#'
#' @name .onLoad
#' @rdname onLoad

.onLoad <- function(libname, pkgname) {  
  cfg_dir <- file.path(path.expand("~"), paste0(".", pkgname))
  if(!file.exists(cfg_dir)) {
    dir.create(cfg_dir, showWarnings=FALSE, mode="0700")
  }
}

.onAttach <- function(libname, pkgname) {
  
}

.onUnload <- function(libpath) {
}

#' onDetach
#'
#' @name .onDetach
#' @rdname onDetach
#' @export

.onDetach <- function(libpath) {
  ## message(".onDetach ", libpath)  
  ##cl <- .initDefaultCluster()
  ##if(!is.null(cl)) {
  ##  stopCluster(cl)  
  ##}
}
#' .Last.lib
#'
#' @name .Last.lib
#' @rdname Last.lib
#' @export

.Last.lib <- function(libpath) {
  ## message(".Last.lib ", libpath)
}
