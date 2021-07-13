#' returns the exponent of a floating point numeber (eng representation)
#'
#' @name exponent
#' @param x number
#' @return the exponent of the eng. representation of the number

exponent <- function(x) {
   if (x == 0) 0
   else floor(log10(abs(x)))
}


identicalts <- function(x, y, toll = 0.000001) {
  all(x - y < toll)
}

ts_differ <- function(x, y, toll = 0.000001) {
  if(toll < 0) warning("toll is negative")
  any(abs(x-y) > toll) ||
    length(x) != length(y) ||
    abs(zoo::index(x)-zoo::index(y)) > toll
}
