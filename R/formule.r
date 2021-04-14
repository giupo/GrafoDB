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
  con <- build_connection()
  on.exit(disconnect(con))

  if (isRoot(g, name)) {
    warning(name, " is a root")
  }

  if (!isNode(g, name)) {
    warning(name, " not in this graph", g@tag)
  }

  df <- DBI::dbGetQuery(con, sql_by_key(helper, "HISTORY_FORMULE", name = name))
  if (nrow(df) == 0) {
    return(data.frame(
      formula = character(0),
      tag = character(0),
      autore = character(0),
      last_updated = character(0)))
  }

  normalize_to_9 <- function(x) rutils::ifelse(exponent(x) == 12, x / 1000, x)

  date <- as.POSIXct(mapply(normalize_to_9, df$last_updated),
                     origin = "1970-01-01")
  df$last_updated <- date
  df <- df[!duplicated(df[, c("formula")]), ]
  df[order(df$last_updated), ]
}
