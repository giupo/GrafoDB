.ser <- function(x, name, debug=FALSE) {
  ## that's the dumbest thing in my life, inverting arguments.
  if(!debug) {
    ret <- .evaluateSingle(name, x)
    if(!is.ts(ret)) {
      stop(name, " non e' un oggetto ts")
    }
    ret
  } else {
    task <- expr(x, name, echo=FALSE)
    if(is.null(task)) {
      if(name %in% names(x)) {
        stop(name, " non e' una serie con formula")
      } else {
        stop(name, " non e' una serie del grafo")
      }
    }
    funcName <- paste0(name, "_func")
    f <- .clutter_function(task, name, funcName=funcName)
    filetmp <- tempfile(pattern=name, fileext=".R")
    write(f, file=filetmp)

    env <- new.env()
    source(filetmp)
    debug(funcName)
    nomi_padri <- deps(x, name)
    if(is.null(nomi_padri) || length(nomi_padri) == 0) {
      padri <- list()
    } else {
      padri <- x[[nomi_padri]]
    }

    if(length(nomi_padri) == 1) {
      ## boxing
      ppp <- list()
      ppp[[nomi_padri]] <- padri
      padri <- ppp
    }
    
    attach(padri)
    on.exit({
      rm(list=c(funcName), envir=globalenv())
      file.remove(filetmp)
      detach(padri)
    })
    eval(parse(text=paste0(funcName, "()")))    
  }
}
