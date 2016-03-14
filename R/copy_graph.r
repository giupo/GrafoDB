.copyGraph <- function(from, to, con=NULL, ...) {
  param_list <- list(...)
  msg <- if('msg' %in% names(param_list)) {
    param_list[["msg"]]
  } else {
    NULL
  }
  
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
    dbGetPreparedQuery(
      con,
      paste0("insert into grafi(tag, commento, last_updated, autore) values ",
             "(?, ?, LOCALTIMESTAMP::timestamp(0), ?)"),
      bind.data = data.frame(to, commento, autore))
    
    ## copia dati
    dbGetPreparedQuery(
      con,
      paste0("insert into dati(tag, name, anno, periodo, freq, dati, autore) ",
             " select ?, name, anno, periodo, freq, dati, ? from dati where tag = ?"),
      bind.data = params)
    ## copia archi
    dbGetPreparedQuery(
      con,
      paste0("insert into archi(tag, partenza, arrivo, autore) ",
             " select ?, partenza, arrivo, ? from archi where tag = ?"),
      bind.data = params)
    ## copia formule
    dbGetPreparedQuery(
      con,
      paste0("insert into formule(tag, name, formula, autore) ",
             " select ?, name, formula, ? from formule where tag = ?"),
      bind.data = params)
    ## copia metadati
    dbGetPreparedQuery(
      con,
      paste0("insert into metadati(tag, name, key, value, autore) ",
             " select ?, name, key, value, ? from metadati where tag = ?"),
      bind.data = params)
    ## inserisce nella tab grafi
    dbGetPreparedQuery(
      con,
      paste0("update grafi set last_updated=LOCALTIMESTAMP::timestamp(0) where tag=?"),
      bind.data = to)
    ## Ricordati di committare.
    if(wasNull) {
      dbCommit(con)
    }
  }, error = function(err) {
    dbRollback(con)
    stop(err)
  })
}
