
.elimina <- function(tag, con, helper) {
  dbExecute(con, getSQLbyKey(helper, "DELETE_GRAFI", tag=tag))
  dbExecute(con, getSQLbyKey(helper, "DELETE_CONFLITTI", tag=tag))
  orig_tables <- c("archi", "dati", "metadati", "formule", "history")
  tables <- paste(orig_tables, tag, sep="_")
  for(table in tables) {
    if(dbExistsTable(con, table)) {
      dbExecute(con, getSQLbyKey(helper, "DROP_TABLE", tab=table)) # nocov      
    }
  }
  for(table in orig_tables) {
    dbExecute(con, paste0("delete from ", table, " where tag='", tag, "'"))
  }
}

#' Elimina un edizione del grafo
#'
#' Cancella dal database un edizione del grafo partendo dal suo `tag`
#'
#' @name elimina
#' @usage elimina(tag)
#' @param tag `tag` che distingue in modo univoco il grafo ed i suoi dati
#' @export
#' @importFrom DBI dbSendQuery dbBegin dbCommit dbRollback dbExistsTable dbExecute
#' @include functions.r
#' @include db.r
#' @include sqlhelper.r

elimina <- function(tag) {

  if(is.grafodb(tag)) {
    tag <- tag@tag
  } else {
    tag <- tolower(tag)
  }

  helper <- SQLHelper()
  con <- pgConnect()
  on.exit(dbDisconnect(con))

  tryCatch({
    dbBegin(con)
    .elimina(tag, con, helper)
    dbCommit(con)
  }, error = function(err) {
    dbRollback(con)
    stop(err)
  })
}
