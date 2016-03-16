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
#' @importFrom DBI dbGetQuery
#' @importFrom RPostgreSQL dbGetQuery

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
  params <- as.data.frame(list(nuovo=nuovo, vecchio=vecchio))
  tryCatch({
    dbBegin(con)
    sqlUpdateDati <- paste0("update dati_", tag," set name = '", nuovo,
                            "' where name = '", vecchio,"'")
    sqlUpdateFormula <- paste0("update formule_", tag, " set name = '", nuovo,
                               "' where name = '", vecchio,"'")
    dbGetQuery(con, sqlUpdateDati)
    dbGetQuery(con, sqlUpdateFormula)
    
    for(figlia in figlie) {
      sql <- paste0("update formule_", tag,
                    " set formula = replace(formula, '", vecchio,"', '", nuovo,
                    "') where name = '", figlia,"'")
      dbGetQuery(con, sql)
    }

    sqlUpdateArchiPartenza <- paste0("update archi_", tag, " set partenza = '", nuovo,
                                     "' where partenza = '", vecchio,"'")
    sqlUpdateArchiArrivo <- paste0("update archi_", tag, " set arrivo = '", nuovo,
                                     "' where arrivo = '", vecchio,"'")
    dbGetQuery(con, sqlUpdateArchiPartenza)
    dbGetQuery(con, sqlUpdateArchiArrivo)
    
    if(dbExistsTable(con, paste0("metadati_",tag))) {
      sqlMetadati <- paste0("update metadati_", tag, " set name = '",nuovo,"' where name = '", vecchio,"'")
      dbGetQuery(con, sqlMetadati)
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
