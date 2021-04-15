#' Elimina un nodo dal `GrafoDB`
#'
#' L'eliminazione prevede l'eliminazione dai dati, formule, archi e metadati
#'
#' @name rm_node_impl
#' @param graph istanza di `GrafoDB`
#' @param ts_name nomi di serie da eliminare
#' @param recursive `TRUE` se l'eliminazione deve essere rivorsiva sugli archi
#'  uscenti di ogni serie nel parametro `ts_name`. `FALSE` altrimenti. Se il
#'  parametro e' impostato a `FALSE` e' condizione necessaria che le serie in
#'  `ts_name` siano tutte foglie, ovvero serie senza archi uscenti
#' @note Metodo interno
#' @seealso rmNode
#' @rdname rmNode-internal

rm_node_impl <- function(graph, ts_name, recursive=FALSE) {
  all_leaves <- isLeaf(graph, ts_name)
  tag <- graph@tag
  if (!recursive && !all_leaves) {
    stop(paste("Non posso cancellare serie intermedie senza",
               "farlo ricorsivamente (ATTENZIONE!)"))
  }

  helper <- graph@helper

  con <- build_connection()
  on.exit(disconnect(con))

  tryCatch({
    DBI::dbBegin(con)

    figlie <- downgrf(graph, ts_name)

    to_be_deleted <- union(figlie, ts_name)

    ## eliminare i dati
    for (name in to_be_deleted) {
      DBI::dbExecute(con, sql_by_key(
        helper, "DELETE_DATI_TAG_NAME",
        tag = tag,
        name = name))
    }

    suppressWarnings(hash::del(to_be_deleted, graph@data))

    network <- graph@network
    ## eliminare i vertici dal grafo
    network <- network - igraph::vertex(to_be_deleted)
    ## eliminare le formule
    for (name in to_be_deleted) {
      DBI::dbExecute(con, sql_by_key(
        helper, "DELETE_FORMULE_TAG_NAME",
        tag = tag,
        name = name))
    }

    suppressWarnings(hash::del(to_be_deleted, graph@functions))
    ## eliminare gli archi
    for (name in to_be_deleted) {
      DBI::dbExecute(con, sql_by_key(
        helper, "DELETE_ARCHI_TAG_PA",
        tag = tag,
        partenza = name,
        arrivo = name))
    }

    ## eliminare i metadati
    for (name in to_be_deleted) {
      DBI::dbExecute(con, sql_by_key(
        helper, "DELETE_META_TAG_NAME",
        tag = tag,
        name = name))
    }


    graph@network <- network
    DBI::dbCommit(con)
  }, error = function(cond) {
    DBI::dbRollback(con)
    stop(cond)
  })

  resync(graph, con = con)
}
