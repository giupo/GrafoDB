#' onLoad
#'
#' @name .onLoad
#' @rdname onLoad

.onLoad <- function(libname, pkgname) {  
  cfg_dir <- file.path(path.expand("~"), paste0(".", pkgname)) # nocov start
  if(!file.exists(cfg_dir)) { 
    dir.create(cfg_dir, showWarnings=FALSE, mode="0700") 
  } # nocov end 
} 

.onAttach <- function(libname, pkgname) {
}

.onUnload <- function(libpath) {
}

.onDetach <- function(libpath) {
}

