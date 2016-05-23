#' @include redis.r 

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
    dbGetQuery(
      con,
      paste0("insert into grafi(tag, commento, last_updated, autore) values ",
             "('", to,"', '", commento,"', LOCALTIMESTAMP::timestamp(0), '", autore, "')"))
    
    ## copia dati
    dbGetQuery(
      con,
      paste0("insert into dati(tag, name, anno, periodo, freq, dati, autore) ",
             " select distinct '", to, "', name, anno, periodo, freq, dati, '", autore,
             "' from dati where tag = '", from, "'"))
    ## copia archi
    dbGetQuery(
      con,
      paste0("insert into archi(tag, partenza, arrivo, autore) ",
             " select distinct '", to, "', partenza, arrivo, '", autore,
             "' from archi where tag = '", from, "'"))
    ## copia formule
    dbGetQuery(
      con,
      paste0("insert into formule(tag, name, formula, autore) ",
             " select distinct '", to, "', name, formula, '", autore,
             "' from formule where tag = '", from, "'"),
      bind.data = params)
    
    ## copia asincrona metadati 
    sendCopyMetadati(from, to)
    
    ## inserisce nella tab grafi
    dbGetQuery(
      con,
      paste0("update grafi set last_updated=LOCALTIMESTAMP::timestamp(0), autore='", autore,
             "' where tag='", to, "'"))
    
    ## Ricordati di committare.
    if(wasNull) {
      dbCommit(con)
    }
  }, error = function(err) {
    dbRollback(con)
    stop(err)
  })
}
