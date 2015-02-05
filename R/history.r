#' Mostra le release precedenti (provvisorio) del grafo
#'
#' La funzione dato il tag, mostra la history di un `GrafoDB`
#' Ovvero tutte le release che hanno sovrascritto la release principale
#' Quella senza p
#'
#' @name showHistory
#' @usage showHistory(x)
#' @param x oggetto R su cui mostrare la history. Puo' essere un istanza
#'          di `GrafoDB` (da cui si prende la tag con `slot(x, 'tag')`), oppure
#'          la `tag` stessa
#' @return un dataframe con le informazioni (autore, last_updated, ordinale)
#' @include core.r db.r
#' @import RPostgreSQL rcf
#' @export

setGeneric(
  "showHistory",
  function(x) {
    standardGeneric("showHistory")
  })

setMethod(
  "showHistory",
  signature("GrafoDB"),
  function(x) {
    showHistory(x@tag)
  })

setMethod(
  "showHistory",
  signature("character"),
  function(x) {
    tag <- x
    con <- pgConnect();
    on.exit(dbDisconnect(con))
    sql <- paste0("select distinct tag, ordinale, autore, max(last_updated) ",
                  " from history where tag=? group by tag, ordinale, autore ",
                  "order by 4,2")
    dbGetPreparedQuery(con, sql, bind.data = tag)
  })
