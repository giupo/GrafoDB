#' edita una serie del grafo
#'
#' @rdname edita-internal
#' @include functions.r
#' @importFrom utils file.edit
#' @importFrom stringr str_trim

.edita <- function(x, name, ...) {
  file <- tempfile(pattern=paste0(name, "-"), fileext=".R")
  new_task <- paste0(name, " = ... # work it")
  if(!isNode(x, name)) {
    deps <- c()
    if(name %in% keys(x@functions)) {
      task <- x@functions[[name]]
    } else {
      task <- new_task
    }
  } else {
    deps <- getDependencies(x, name)
    task <- expr(x, name, echo=F)
    if(is.null(task)) {
      stop("la serie ", name, " e' una serie primitiva")
    }
  }
  old_deps <- deps
  old_task <- task
  if(name %in% keys(x@edges)) {
    deps <- x@edges[[name]]
  }  
  
  task <- .clutter_with_params(task, name, deps) 
  write(task, file=file)
  on.exit(file.remove(file))
  file.edit(file, title=name)
  txtsrc <- paste(readLines(file), collapse="\n")
  edited <- .declutter_function(txtsrc)
  
  #if(str_trim(edited) == str_trim(old_task)) {
  #  return(invisible(x))
  #}
  
  params <- list(...)
  tryCatch({
    f <- eval(parse(text=txtsrc))
    dep <- names(as.list(formals(f)))
    if(str_trim(edited) == str_trim(old_task) &&
         identical(sort(dep), sort(old_deps))) {
      ## non sono cambiati formula e archi, non faccio niente
      
      return(invisible(x))
    }
    
    x@functions[name] <- edited

    if(!is.null(dep)) {
      x@edges[[name]] <- dep
    }

    x[name] <- f
    invisible(x)
  }, error = function(cond) {
    ## la risetto per poterla editare
    x@functions[name] <- edited
    stop(cond)
  })
}
