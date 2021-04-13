#' edita una serie del grafo
#'
#' @rdname edita-internal
#' @include functions.r

.edita <- function(x, name, ...) {
  file <- tempfile(pattern = paste0(name, "-"), fileext = ".R")
  new_task <- paste0(name, " = ... # work it")
  is_node <- isNode(x, name)
  if(! is_node) {
    deps <- c()
    if(name %in% hash::keys(x@functions)) {
      task <- x@functions[[name]]
    } else {
      task <- new_task
    }
  } else {
    deps <- get_deps(x, name)
    task <- expr(x, name, echo = FALSE)
    if(is.null(task)) {
      warning("la serie ", name, " e' una serie primitiva")
      task <- new_task
    }
  }

  old_deps <- deps
  old_task <- task
  if(name %in% hash::keys(x@edges)) {
    deps <- x@edges[[name]]
  }

  task <- clutter_with_params(task, name, deps)
  write(task, file = file)
  on.exit(file.remove(file))
  utils::file.edit(file, title = name)
  txtsrc <- paste(readLines(file), collapse = "\n")
  edited <- declutter_function(txtsrc)

  params <- list(...)
  tryCatch({
    f <- eval(parse(text = txtsrc))
    dep <- names(as.list(formals(f)))

    is_same_function <- stringr::str_trim(edited) ==
      stringr::str_trim(old_task)

    has_same_edges <- identical(sort(dep), sort(old_deps))

    if (is_same_function && has_same_edges) {
      ## non sono cambiati archi: non faccio niente
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
    if(stringr::str_trim(edited) != stringr::str_trim(old_task)) {
      x@functions[name] <- edited
    }
    stop(cond)
  })
}
