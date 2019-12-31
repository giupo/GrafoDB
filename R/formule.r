#' This programs shows all the editions of a formula
#'
#' @name formule
#' @usage formule(g, name)
#' @param g GrafoDB instance
#' @param name name of the object to be inspected
#' @return a data.frame with unique formulas and `tag`
#' @include utils.r
#' @export

formule <- function(g, name) {
  helper <- g@helper
  con <- buildConnection()
  on.exit(disconnect(con))

  if(isRoot(g, name)) {
    warning(name, " e' una radice")
  }

  if(!isNode(g, name)) {
    warning(name, " non e' un oggetto del Grafo ", g@tag)
  }

  df <- dbGetQuery(con, getSQLbyKey(helper, "HISTORY_FORMULE", name=name))
  if(nrow(df) == 0) {
    data.frame(formula=character(0), tag=character(0), 
       autore=character(0), last_updated=character(0))
  } else {
    normalizeTo9 <- function(x) {
      if(exponent(x) == 12) x / 1000
      else x
    }

    date <- as.POSIXct(mapply(normalizeTo9, df$last_updated), origin="1970-01-01")
    df$last_updated <- date
    df <- df[!duplicated(df[, c("formula")]),]
    df[order(df$last_updated), ]
  }
}
