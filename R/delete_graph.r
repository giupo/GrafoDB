
.delete_graph <- function(tag, con, helper) {
  DBI::dbExecute(con, sql_by_key(helper, "DELETE_GRAFI", tag=tag))
  DBI::dbExecute(con, sql_by_key(helper, "DELETE_CONFLITTI", tag=tag))
  orig_tables <- c("archi", "dati", "metadati", "formule")
  tables <- paste(orig_tables, tag, sep="_")
  for(table in tables) {
    if(DBI::dbExistsTable(con, table)) {
      DBI::dbExecute(con, sql_by_key(helper, "DROP_TABLE", tab=table)) # nocov      
    }
  }
  for(table in orig_tables) {
    DBI::dbExecute(con, paste0("delete from ", table, " where tag='", tag, "'"))
  }
}

#' Deletes a Graph from the DB
#'
#' Deletes a graph by its tag
#'
#' @name delete_graph
#' @usage delete_graph(tag)
#' @param tag `tag` as the primary key of the Graph
#' @export
#' @include functions.r
#' @include db.r
#' @include sqlhelper.r

delete_graph <- function(tag) {
  tag <- if(is.grafodb(tag)) {
    tag <- tag@tag
  } else {
    tag <- tolower(tag)
  }

  helper <- SQLHelper()
  con <- buildConnection()
  on.exit(disconnect(con))

  tryCatch({
    DBI::dbBegin(con)
    .delete_graph(tag, con, helper)
    DBI::dbCommit(con)
  }, error = function(err) {
    DBI::dbRollback(con)
    stop(err)
  })
}
