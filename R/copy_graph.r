#' @include redis.r sqlhelper.r

.copyGraph <- function(from, to, con=NULL, ...) {
  param_list <- list(...)
  msg <- if('msg' %in% names(param_list)) {
    param_list[["msg"]]
  } else {
    NULL
  }
  helper <- SQLHelper()
  if(is.null(con)) {
    wasNull <- TRUE
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    tryCatch({
      dbBegin(con)
    }, error = function(cond) {
      dbRollback(con)
      stop(cond)
    })
  } else {
    wasNull <- FALSE
  }
  
  commento <- paste0("Rilascio per ", to)
  autore <- whoami()
  params <- cbind(to, autore, from)
  tryCatch({    
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
    
    ## Ricordati di committare.
    if(wasNull) {
      dbCommit(con)
    }
  }, error = function(err) {
    dbRollback(con)
    stop(err)
  })
}
