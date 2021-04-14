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