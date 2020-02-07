
.elimina <- function(tag, con, helper) {
  DBI::dbExecute(con, getSQLbyKey(helper, "DELETE_GRAFI", tag=tag))
  DBI::dbExecute(con, getSQLbyKey(helper, "DELETE_CONFLITTI", tag=tag))
  orig_tables <- c("archi", "dati", "metadati", "formule")
  tables <- paste(orig_tables, tag, sep="_")
  for(table in tables) {
    if(DBI::dbExistsTable(con, table)) {
      DBI::dbExecute(con, getSQLbyKey(helper, "DROP_TABLE", tab=table)) # nocov      
    }
  }
  for(table in orig_tables) {
    DBI::dbExecute(con, paste0("delete from ", table, " where tag='", tag, "'"))
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
  con <- buildConnection()
  on.exit(disconnect(con))

  tryCatch({
    DBI::dbBegin(con)
    .elimina(tag, con, helper)
    DBI::dbCommit(con)
  }, error = function(err) {
    DBI::dbRollback(con)
    stop(err)
  })
}
