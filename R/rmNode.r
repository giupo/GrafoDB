#' Elimina un nodo dal `GrafoDB`
#'
#' L'eliminazione prevede l'eliminazione dai dati, formule, archi e metadati
#'
#' @name .rmNode
#' @usage .rmNode(graph, tsName, recursive)
#' @param graph istanza di `GrafoDB`
#' @param tsName nomi di serie da eliminare
#' @param recursive `TRUE` se l'eliminazione deve essere rivorsiva sugli archi
#'                  uscenti di ogni serie nel parametro `tsName`.
#'                  `FALSE` altrimenti. Se il parametro e' impostato a `FALSE` e'
#'                  condizione necessaria che le serie in `tsName` siano tutte
#'                  foglie, ovvero serie senza archi uscenti
#' @note Metodo interno
#' @seealso rmNode
#' @rdname rmNode-internal

.rmNode <- function(graph, tsName, recursive=FALSE) {
  sono.tutte.foglie <- isLeaf(graph, tsName)
  tag <- graph@tag
  if(!recursive && !sono.tutte.foglie) {
    stop(paste("Non posso cancellare serie intermedie senza",
               "farlo ricorsivamente (ATTENZIONE!)"))
  }
  
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  tryCatch({  
    dbBegin(con)
    
    figlie <- downgrf(graph, tsName)
    
    da.eliminare <- union(figlie, tsName)
    
    ## eliminare i dati
    for(name in da.eliminare) {
      dbGetQuery(
        con,
        paste0("delete from dati where tag='", tag, "' and name='", name, "'"))
    }
    
    suppressWarnings(del(da.eliminare, graph@data))
    
    network <- graph@network
    ## eliminare i vertici dal grafo
    network <- network - vertex(da.eliminare)
    ## eliminare le formule
    for(name in da.eliminare) {
      dbGetQuery(
        con,
        paste0("delete from formule where tag='", tag, "' and name='", name , "'"))
    }
    
    suppressWarnings(del(da.eliminare, graph@functions))
    ## eliminare gli archi
    for(name in da.eliminare) {
      dbGetQuery(
        con,
        paste0("delete from archi where tag='", tag, "'",
               " and (partenza='", name, "' or arrivo='", name, "')"))
      
    }
    
    ## eliminare i metadati
    for(name in da.eliminare) {
      dbGetQuery(
        con,
        paste0("delete from metadati where tag='", tag, "' and name='", name, "'"))
    }
    
    graph@network <- network
    dbCommit(con)
  }, error = function(cond) {
    dbRollback(con)
    stop(cond)
  })
  resync(graph, con=con)
}
