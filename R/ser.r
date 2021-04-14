ser_impl <- function(x, name, debug=FALSE) {
  ## that's the dumbest thing in my life, inverting arguments.
  if (!debug) {
    ret <- evaluate_single(name, x)
    if (!stats::is.ts(ret)) {
      stop(name, " non e' un oggetto ts")
    }
    ret
  } else {
    task <- expr(x, name, echo = FALSE)
    if (is.null(task)) {
      if (name %in% names(x)) {
        stop(name, " non e' una serie con formula")
      } else {
        stop(name, " non e' una serie del grafo")
      }
    }
    func_name <- paste0(name, "_func")
    f <- to_function_as_string(task, name, func_name = func_name)
    filetmp <- tempfile(pattern = name, fileext = ".R")
    write(f, file = filetmp)

    env <- new.env()
    source(filetmp)
    debug(func_name)
    nomi_padri <- deps(x, name)
    if (is.null(nomi_padri) || length(nomi_padri) == 0) {
      padri <- list()
    } else {
      padri <- x[[nomi_padri]]
    }

    if (length(nomi_padri) == 1) {
      ## boxing
      ppp <- list()
      ppp[[nomi_padri]] <- padri
      padri <- ppp
    }

    attach(padri)

    on.exit({
      rm(list = c(func_name), envir = parent.frame())
      file.remove(filetmp)
      detach(padri)
    })

    eval(parse(text = paste0(func_name, "()")))
  }
}
