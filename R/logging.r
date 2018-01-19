

#' @importFrom crayon red green blue yellow silver 

layout.colored <- function(level, msg, ...) {
  the.time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (length(list(...)) > 0) {
    parsed <- lapply(list(...), function(x) if(is.null(x)) 'NULL' else x )
    msg <- do.call(sprintf, c(msg, parsed))
  }

  proxy.message <- crayon::white
  nlevel <- names(level)

  proxy.level <- if(nlevel == "FATAL") {
    red
  } else if (nlevel == "ERROR") {
    red
  } else if (nlevel == "WARN") {
    yellow
  } else if (nlevel == "INFO") {
    white
  } else if (nlevel == "DEBUG") {
    green
  } else {
    silver
  }
  
  sprintf("%s [%s] %s\n", proxy.level(names(level)), the.time, proxy.message(msg))
}
