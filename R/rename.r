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
#' @importFrom DBI dbGetQuery dbExecute
#' @importFrom igraph get.vertex.attribute set.vertex.attribute

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
  if(vecchio %in% keys(data) || vecchio %in% keys(functions) ||
       any(figlie %in% keys(data)) ||
       any(figlie %in% keys(functions))) {
    stop(vecchio, " o figlie di ", vecchio,
         " sono in modifica. Salvare prima le modifiche ed in seguito rinominare le serie")
  }
  
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  
  tag <- x@tag
  helper <- x@helper
  params <- as.data.frame(list(nuovo=nuovo, vecchio=vecchio))
  tryCatch({
    dbBegin(con)

    dbExecute(con, getSQLbyKey(
      helper, "RENAME_DATI",
      tag=tag,
      nuovo=nuovo,
      vecchio=vecchio))
    
    dbExecute(con, getSQLbyKey(
      helper, "RENAME_FORMULE",
      tag=tag,
      nuovo=nuovo,
      vecchio=vecchio))
    
    for(figlia in figlie) {
      dbExecute(con, getSQLbyKey(
        helper, "RENAME_FORMULA",
        tag=tag,
        vecchio=vecchio,
        nuovo=nuovo,
        figlia=figlia))
    }

    dbExecute(con, getSQLbyKey(
      helper, "RENAME_ARCHI_PARTENZA",
      nuovo=nuovo,
      vecchio=vecchio,
      tag=tag))

    dbExecute(con, getSQLbyKey(
      helper, "RENAME_ARCHI_ARRIVO",
      nuovo=nuovo,
      vecchio=vecchio,
      tag=tag))
    
    if(dbExistsTable(con, paste0("metadati_",tag))) {
      dbExecute(con, getSQLbyKey(
        helper, "RENAME_METADATI",
        nuovo=nuovo,
        vecchio=vecchio,
        tag=tag))
    }
    
    
    nomiarchi <- get.vertex.attribute(x@network, "name")
    nomiarchi[nomiarchi == vecchio] <- nuovo
    x@network <- set.vertex.attribute(x@network, "name", value=nomiarchi)
    x <- resync(x, con=con)    
    dbCommit(con)
    x
  }, error = function(cond) {
    dbRollback(con)
    stop(cond)
  })
}
