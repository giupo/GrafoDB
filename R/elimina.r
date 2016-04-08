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

elimina <- function(tag) {

  if(is.grafodb(tag)) {
    tag <- tag@tag
  }

  incancellabili <- c("cf10")
  if(tag %in% incancellabili) stop("Non cancellero' mai ", tag)
 
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  tryCatch({
     dbBegin(con)
     
     dbGetQuery(con, paste0("delete from grafi where tag='", tag, "'"))
     dbGetQuery(con, paste0("delete from conflitti where tag='", tag, "'"))
     
     tables <- c("archi", "dati", "metadati", "formule", "history")
     tables <- paste(tables, tag, sep="_")

     dbGetQuery(con, paste0("delete from conflitti where tag='", tag,"'"))
     
     for(table in tables) {
       if(dbExistsTable(con, table)) {
         dbGetQuery(con, paste0("drop table if exists ", table))
       }
     }
  }, error = function(err) {
    dbRollback(con)
    stop(err)
  })
  dbCommit(con)
}
