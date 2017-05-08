

#' diff implementation for GrafoDB
#'
#' Returns diffs as data and formulas
#' 
#' @export
#' @method diff GrafoDB

diff.GrafoDB <- function(x, ...) {
  y <- list(...)[[1]]
  helper <- x@helper
  
  stopifnot(is.grafodb(y))

  not_common <- union(
    setdiff(names(x), names(y)),
    setdiff(names(y), names(x)))
  if(length(not_common)) {
    warning("not common names: ", paste0(not_common, collapse=", "))
  }

  con <- pgConnect()
  on.exit(dbDisconnect(con))

  df <- dbGetQuery(con, getSQLbyKey(helper,
                                    "DIFF_FORMULE",
                                    new = x@tag,
                                    old = y@tag))

  colnames(df) <- c("name", x@tag, y@tag)
  df
}
