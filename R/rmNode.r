#' Elimina un nodo dal `GrafoDB`
#'
#' L'eliminazione prevede l'eliminazione dai dati, formule, archi e metadati
#'
#' @name rm_node_impl
#' @usage rm_node_impl(graph, ts_name, recursive)
#' @param graph istanza di `GrafoDB`
#' @param ts_name nomi di serie da eliminare
#' @param recursive `TRUE` se l'eliminazione deve essere rivorsiva sugli archi
#'                  uscenti di ogni serie nel parametro `ts_name`.
#'                  `FALSE` altrimenti. Se il parametro e' impostato a `FALSE` e'
#'                  condizione necessaria che le serie in `ts_name` siano tutte
#'                  foglie, ovvero serie senza archi uscenti
#' @note Metodo interno
#' @seealso rmNode
#' @rdname rmNode-internal

rm_node_impl <- function(graph, ts_name, recursive=FALSE) {
  sono.tutte.foglie <- isLeaf(graph, ts_name)
  tag <- graph@tag
  if (!recursive && !sono.tutte.foglie) {
    stop(paste("Non posso cancellare serie intermedie senza",
               "farlo ricorsivamente (ATTENZIONE!)"))
  }

  helper <- graph@helper

  con <- build_connection()
  on.exit(disconnect(con))

  tryCatch({
    DBI::dbBegin(con)

    figlie <- downgrf(graph, ts_name)

    da.eliminare <- union(figlie, ts_name)

    ## eliminare i dati
    for (name in da.eliminare) {
      DBI::dbExecute(con, sql_by_key(
        helper, "DELETE_DATI_TAG_NAME", tag = tag, name=name))
    }

    suppressWarnings(hash::del(da.eliminare, graph@data))

    network <- graph@network
    ## eliminare i vertici dal grafo
    network <- network - igraph::vertex(da.eliminare)
    ## eliminare le formule
    for (name in da.eliminare) {
      DBI::dbExecute(con, sql_by_key(
        helper, "DELETE_FORMULE_TAG_NAME", tag = tag, name=name))
    }

    suppressWarnings(hash::del(da.eliminare, graph@functions))
    ## eliminare gli archi
    for (name in da.eliminare) {
      DBI::dbExecute(con, sql_by_key(
        helper, "DELETE_ARCHI_TAG_PA", tag = tag, partenza=name, arrivo=name))

    }

    ## eliminare i metadati
    for (name in da.eliminare) {
      DBI::dbExecute(con, sql_by_key(
        helper, "DELETE_META_TAG_NAME", tag = tag, name=name))
    }


    graph@network <- network
    DBI::dbCommit(con)
  }, error = function(cond) {
    DBI::dbRollback(con)
    stop(cond)
  })

  resync(graph, con = con)
}
