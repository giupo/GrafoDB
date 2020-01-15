#' returns the exponent of a floating point numeber (eng representation)
#' 
#' @name exponent
#' @return the exponent of the eng. representation of the number

exponent <- function(x) {
   if (x == 0) 0
   else floor(log10(abs(x)))
} 