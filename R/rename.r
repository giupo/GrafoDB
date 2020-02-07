#' Rinomina una serie del grafo
#'
#' L'operazione lavora direttamente sui dati in modo persistente.
#'
#' @name .rename
#' @usage .rename(x, vecchio, nuovo)
#' @rdname rename-internal
#' @param x istanza di `GrafoDB`
#' @param vecchio nome vecchio da sostituire
#' @param nuovo nome nuovo da sostituire
#' @return grafo modificato
#' @include db.r functions.r

.rename <- function(x, vecchio, nuovo) {
  if(isNode(x, nuovo)) {
    stop(nuovo, " e' gia' una serie del grafo")
  }
  
  if(!isNode(x, vecchio)) {
    stop(vecchio, " non e' una serie del grafo")
  }
  
  figlie <- downgrf(x, vecchio, livello=1)
  data <- x@data
  functions <- x@functions
  if(vecchio %in% hash::keys(data) || vecchio %in% hash::keys(functions) ||
       any(figlie %in% hash::keys(data)) ||
       any(figlie %in% hash::keys(functions))) {
    stop(vecchio, " o figlie di ", vecchio,
         " sono in modifica. Salvare prima le modifiche ed in seguito rinominare le serie")
  }
  
  con <- buildConnection()
  on.exit(disconnect(con))
  
  tag <- x@tag
  helper <- x@helper
  params <- as.data.frame(list(nuovo=nuovo, vecchio=vecchio))
  tryCatch({
    DBI::dbBegin(con)

    DBI::dbExecute(con, getSQLbyKey(
      helper, "RENAME_DATI",
      tag=tag,
      nuovo=nuovo,
      vecchio=vecchio))
    
    DBI::dbExecute(con, getSQLbyKey(
      helper, "RENAME_FORMULE",
      tag=tag,
      nuovo=nuovo,
      vecchio=vecchio))
    
    for(figlia in figlie) {
      DBI::dbExecute(con, getSQLbyKey(
        helper, "RENAME_FORMULA",
        tag=tag,
        vecchio=vecchio,
        nuovo=nuovo,
        figlia=figlia))
    }

    DBI::dbExecute(con, getSQLbyKey(
      helper, "RENAME_ARCHI_PARTENZA",
      nuovo=nuovo,
      vecchio=vecchio,
      tag=tag))

    DBI::dbExecute(con, getSQLbyKey(
      helper, "RENAME_ARCHI_ARRIVO",
      nuovo=nuovo,
      vecchio=vecchio,
      tag=tag))
    
    if(DBI::dbExistsTable(con, paste0("metadati_",tag))) {
      DBI::dbExecute(con, getSQLbyKey(
        helper, "RENAME_METADATI",
        nuovo=nuovo,
        vecchio=vecchio,
        tag=tag))
    }
        
    nomiarchi <- igraph::get.vertex.attribute(x@network, "name")
    nomiarchi[nomiarchi == vecchio] <- nuovo
    x@network <- igraph::set.vertex.attribute(x@network, "name", value=nomiarchi)
    x <- resync(x, con=con)    
    DBI::dbCommit(con)
    x
  }, error = function(cond) {
    DBI::dbRollback(con)
    stop(cond)
  })
}
