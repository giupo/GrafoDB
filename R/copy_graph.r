#' @include redis.r sqlhelper.r
.copyGraph <- function(from, to, con, ...) {
  param_list <- list(...)
  msg <- if('msg' %in% names(param_list)) {
    param_list[["msg"]]
  } else {
    ""
  }
  helper <- SQLHelper()
  
  commento <- paste0("Rilascio per ", to)
  autore <- whoami()
  params <- cbind(to, autore, from)
  
  ## copia dati
  dbGetQuery(con, getSQLbyKey(helper, "COPY_DATI", to=to, from=from))
  
  ## copia archi
  dbGetQuery(con, getSQLbyKey(helper, "COPY_ARCHI", to=to, from=from))
  
  ## copia formule
  dbGetQuery(con, getSQLbyKey(helper, "COPY_FORMULE", to=to, from=from))
  
  ## copia asincrona metadati 
  sendCopyMetadati(from, to)
  
  dbGetQuery(con, getSQLbyKey(
    helper, "INSERT_GRAFI", tag=to, commento=commento, autore=autore
  ))
}
