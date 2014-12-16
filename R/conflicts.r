
#' Ritorna `TRUE` se il grafo ha conflitti
#'
#' @name hasConflicts
#' @usage hasConflicts(x)
#' @usage hasConflicts(x, name)
#' @param x oggetto R
#' @param name character array di nomi (puo' essere omesso)
#' @return `TRUE` se l'istanza `x` e' un GrafoDB con conflitti, `FALSE` altrimenti
#' @examples \dontrun{
#' g <- GrafoDB(...)
#' hasConflicts(g) # dovrebbe essere FALSE
#' ...             # eseguo operazioni come modificare la stessa serie
#' hasConflicts(g) # TRUE
#' hasConflicts(g, SERIE_CON_CONFLITTO) # TRUE
#' hasConflicts(g, SERIE_SENZA_CONFLITTO) # FALSE
#' }
#' @export
#' @include core.r

setGeneric(
  "hasConflicts",
  function(x, name=NULL) {
    standardGeneric("hasConflicts")
  })

#' Ritorna `TRUE` se il grafo ha conflitti
#'
#' @name hasConflicts
#' @usage hasConflicts(x)
#' @usage hasConflicts(x, name)
#' @param x oggetto R
#' @param name character array di nomi (puo' essere omesso)
#' @return `TRUE` se l'istanza `x` e' un GrafoDB con conflitti, `FALSE` altrimenti
#' @examples \dontrun{
#' g <- GrafoDB(...)
#' hasConflicts(g) # dovrebbe essere FALSE
#' ...             # eseguo operazioni come modificare la stessa serie
#' hasConflicts(g) # TRUE
#' hasConflicts(g, SERIE_CON_CONFLITTO) # TRUE
#' hasConflicts(g, SERIE_SENZA_CONFLITTO) # FALSE
#' }
#' @export
#' @import rcf
#' @include core.r

setMethod(
  "hasConflicts",
  signature("GrafoDB", "ANY"),
  function(x, name=NULL) {
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    tag <- x@tag
    df <- if(is.character(name)) {
      dbGetPreparedQuery(
        con,
        "select name from conflitti where tag = ? and name = ?",
        bind.data=cbind(tag, name))
    } else {
      dbGetPreparedQuery(
        con,
        "select name from conflitti where tag = ?",
        bind.data=tag)
    }

    nrow(df) > 0
  })

setGeneric(
  "getConflicts",
  function(x, name=NULL) {
    standardGeneric("getConflicts")
  })

