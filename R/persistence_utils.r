#' Legge tutta la tabella dei dati per un determinato tag
#'
#' @name loadTable
#' @usage loadTable(tableName, tag)
#' @usage loadTable(tableName, tag, con)
#' @return un data.frame con i dati delle serie storiche
#' @param tableName nome della tabella
#' @param tag nome del tag
#' @note funzione interna
#' @importFrom DBI dbReadTable
#' @include db.r

loadTable <- function(tableName, tag, con=NULL) {
  conWasNull <- is.null(con)
  con <- pgConnect(con=con)
  if(conWasNull) {
    on.exit(dbDisconnect(con))
  }
  fullTableName <- paste0(tableName, '_', tag)
  if(dbExistsTable(con, fullTableName)) {
    dbReadTable(con, fullTableName)
  } else {
    stop()
  }
}

loadDati <- function(tag, con=NULL) tryCatch({
  loadTable('dati', tag, con=con)
}, error=function(cond) {
  data.frame(
    name=character(),
    year=integer(),
    period=integer(),
    freq=integer(),
    dati=character(),
    stato=integer(),
    notes=character(),
    autore=character())
})

loadArchi <- function(tag, con=NULL) tryCatch({
  loadTable('archi', tag, con=con)
}, error=function(cond) {
  data.frame(partenza=character(), arrivo=character())
})

loadMetadati <- function(tag, con=NULL) loadTable('metadati', tag, con=con)
loadFormule <- function(tag, con=NULL) tryCatch({
  loadTable('formule', tag, con=con)
}, error=function(cond) {
  data.frame(
    name=character(),
    tag=character(),
    formula=character(),
    autore=character())
})

loadGrafi <- function(con=NULL) {
  conWasNull <- is.null(con)
  con <- pgConnect(con=con)
  if(conWasNull) {
    on.exit(dbDisconnect(con))
  }
  dbReadTable(con, 'grafi')
}


createNewGrafo <- function(x, tag, con=NULL) {
  conWasNull <- is.null(con)
  con <- pgConnect(con=con)
  if(conWasNull) {
    on.exit(dbDisconnect(con))
  }
  
  x@timestamp <- Sys.time()
  sql <- paste0(
    "INSERT INTO grafi(tag, commento, last_updated, autore) ",
    " select ?,?,LOCALTIMESTAMP::timestamp(0),? ",
    " WHERE NOT EXISTS (SELECT 1 FROM grafi WHERE tag=?)")
  autore <- whoami()
  dati <- cbind(tag, paste0('Grafo per ', tag), autore, tag)
  names(dati) <- c("tag", "commento", "autore", "tag")
  dbGetPreparedQuery(con, sql, bind.data = dati)
  x
}


resync <- function(x, con=NULL) {
  conWasNull <- is.null(con)
  con <- pgConnect(con=con)
  if(conWasNull) {
    on.exit(dbDisconnect(con))
  }
  tag <- x@tag
  x@dbdati <- loadDati(tag, con=con)
  x@dbformule <- loadFormule(tag, con=con)
  x
}

need_resync <- function(x) {
  timeStamp <- x@timestamp
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  
  sql <- paste0("select count(tag) from grafi where tag='", x@tag,
                "' and last_updated > '", as.character(timeStamp), "'")

  df <- dbGetQuery(con, sql)
  df[[1]] > 0
}
