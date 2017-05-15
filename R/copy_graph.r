#' @include redis.r sqlhelper.r
#' @importFrom R.utils System
.copyGraph <- function(from, to, con, ...) {
  param_list <- list(...)
  commento <- if('msg' %in% names(param_list)) {
    param_list[["msg"]]
  } else {
    paste0("Rilascio per ", to)
  }
  helper <- SQLHelper()

  autore <- if('autore' %in% names(param_list)) {
    param_list[["autore"]]
  } else {
    whoami()
  }
  params <- cbind(to, autore, from)

  ## copia archi
  dbGetQuery(con, getSQLbyKey(helper, "COPY_DATI", to=to, from=from))

  ## copia archi
  dbGetQuery(con, getSQLbyKey(helper, "COPY_ARCHI", to=to, from=from))
  
  ## copia formule
  dbGetQuery(con, getSQLbyKey(helper, "COPY_FORMULE", to=to, from=from))

  ## copio metadati
  dbGetQuery(con, getSQLbyKey(helper, "COPY_METADATI", to=to, from=from))
  
  ## copia asincrona metadati 
  ## sendCopyMetadati(from, to)
  
  dbGetQuery(con, getSQLbyKey(
    helper, "INSERT_GRAFI", tag=to, commento=commento, autore=autore,
    last_updated=round(R.utils::System$currentTimeMillis())))
}
