#' Elimina un edizione del grafo
#'
#' Cancella dal database un edizione del grafo partendo dal suo `tag`
#'
#' @name elimina
#' @usage elimina(tag)
#' @param tag `tag` che distingue in modo univoco il grafo ed i suoi dati
#' @export
#' @importFrom RPostgreSQL2 dbBegin
#' @importFrom RPostgreSQL dbGetQuery
#' @importFrom DBI dbSendQuery dbBegin dbCommit dbRollback dbExistsTable
#' @include functions.r
#' @include db.r
#' @include sqlhelper.r

elimina <- function(tag) {

  if(is.grafodb(tag)) {
    tag <- tag@tag
  }

  incancellabili <- c("cf10")
  if(tag %in% incancellabili) stop("Non cancellero' mai ", tag)

  helper <- SQLHelper()
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  tryCatch({
     dbBegin(con)
     
     dbGetQuery(con, getSQLbyKey(helper, "DELETE_GRAFI", tag=tag))
     dbGetQuery(con, getSQLbyKey(helper, "DELETE_CONFLITTI", tag=tag))
     
     tables <- c("archi", "dati", "metadati", "formule", "history")
     tables <- paste(tables, tag, sep="_")
     
     for(table in tables) {
       if(dbExistsTable(con, table)) {
         dbGetQuery(
           con,
           getSQLbyKey(helper, "DROP_TABLE", tab=table))
       }
     }
  }, error = function(err) {
    dbRollback(con)
    stop(err)
  })
  dbCommit(con)
}
