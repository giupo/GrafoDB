#' This programs shows all the editions of a particular formula
#'
#' @name formule
#' @usage formule(g, name)
#' @param g GrafoDB instance
#' @param name name of the object to be inspected
#' @return a timeseries with unique formulas and `tag` edition
#' @export

formule <- function(g, name) {
  con <- buildConnection()
  on.exit(disconnect(con))

  if(isRoot(g, name)) {
    warning(name, " e' una radice")
  }

  if(!isNode(g, name)) {
    warning(name, " non e' un oggetto del Grafo ", g@tag)
  }

  sql <- paste0(
    "select distinct formula, tag, last_updated ",
    "from formule where name = '", name, "' order by last_updated")

  df <- dbGetQuery(con, sql)

  if(nrow(df) == 0) {
    data.frame(formula=character(0), tag=character(0))
  } else {
    df <- df[, c("formula", "tag")]
    df[!duplicated(df[, c("formula")]),]
  }
}
