

#' diff implementation for GrafoDB
#'
#' Returns diffs as data and formulas
#' 
#' @export
#' @method diff GrafoDB
#' @include logging.r

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

  con <- buildConnection()
  on.exit(disconnect(con))
  sql <- getSQLbyKey(helper, "DIFF_FORMULE", new=x@tag, old=y@tag)
  trace("Diff query: %s", sql, name='GrafoDB.diff')
  df <- DBI::dbGetQuery(con, sql)

  if(nrow(df)) {
    colnames(df) <- c("name", x@tag, paste0(x@tag, "_autore"),
                      paste0(x@tag, "_last_updated"),
                      y@tag, paste0(y@tag, "_autore"),
                      paste0(y@tag, "_last_updated"))
  }
  df
}
