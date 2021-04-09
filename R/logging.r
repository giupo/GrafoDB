layout.colored <- function(level, msg, ...) {
  the.time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (length(list(...)) > 0) {
    parsed <- lapply(list(...), function(x) if(is.null(x)) 'NULL' else x )
    msg <- do.call(sprintf, c(msg, parsed))
  }

  proxy.message <- crayon::white
  nlevel <- names(level)

  proxy.level <- if(nlevel == "FATAL") {
    crayon::red
  } else if (nlevel == "ERROR") {
    crayon::red
  } else if (nlevel == "WARN") {
    crayon::yellow
  } else if (nlevel == "INFO") {
    crayon::white
  } else if (nlevel == "DEBUG") {
    crayon::green
  } else {
    crayon::silver
  }
  
  sprintf("%s [%s] %s\n", proxy.level(names(level)), the.time, proxy.message(msg))
}

# adapter for the logging system...

info <- function(...) {
  futile.logger::flog.info(...)
}

debug <- function(...) {
  futile.logger::flog.debug(...)
}

error <- function(...) {
  futile.logger::flog.error(...)
}

warn <-  function(...) {
  futile.logger::flog.warn(...)
}

trace <- function(...) {
  futile.logger::flog.trace(...)
}

fatal <- function(...) {
  futile.logger::flog.fatal(...)
}