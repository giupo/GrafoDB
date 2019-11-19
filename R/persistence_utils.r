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
  con <- if(is.null(con)) {
    cc <- buildConnection()
    on.exit(disconnect(cc))
    cc
  } else {
    con
  }
  
  fullTableName <- if(class(con) != "SQLiteConnection") {
    paste0(tableName, '_', tag) # nocov # e' valido solo su Postgres con table partition
  } else {
    tableName
  }
  
  df <- if(dbExistsTable(con, fullTableName)) {
    ## FIXME: not really smart to load the whole table in memory when you need just a tag
    ## BUT it's only valid for test environments running SQLite.
    dbReadTable(con, fullTableName)
  } else {
    stop(fullTableName, " non esiste")
  }

  # filter out in case of RSQLite
  df <- df[df$tag == tag, ]
  
  unique(df)
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
  con <- if(is.null(con)) {
    con <- buildConnection()
    on.exit(disconnect(con))
    con
  } else {
    con
  }
  dbReadTable(con, 'grafi')
}


#' @importFrom R.utils System
createNewGrafo <- function(x, tag, con=NULL, msg=paste0('Grafo per ', tag)) {
  autore <- whoami()
  # FIXME: Devo usare i timestamp di R o del DBMS?
  x@timestamp <- time.in.millis()
  helper <- x@helper
  sql <- getSQLbyKey(
    helper, "CREATE_NEW_GRAFO", tag=tag,
    commento=msg, autore=autore,
    last_updated=x@timestamp)
  

  con <- if (is.null(con)) {
    con <- buildConnection()
    on.exit({
      disconnect(con)
    })
    con
  } else {
    con
  }

  tryCatch({
    dbBegin(con)
    dbExecute(con, sql)
    dbCommit(con)
  }, error = function(cond) {
    tryCatch(dbRollback(con), error= function(cx) {
      stop(cx, ", Root: ", cond)
    })
    stop(cond)
  })
  x
}


resync <- function(x, con=NULL) {
  if(is.null(con)) {
    con <- buildConnection()
    on.exit(disconnect(con))
    con
  } else {
    con
  }
  
  tag <- x@tag
  x@dbdati <- loadDati(tag, con=con)
  x@dbformule <- loadFormule(tag, con=con)
  ## e gli archi :?? :)
  x
}


need_resync <- function(x) {
  timeStamp <- x@timestamp
  helper <- x@helper
  con <- buildConnection()
  on.exit(disconnect(con))
  tag <- x@tag
  df <- dbGetQuery(con, getSQLbyKey(
    helper, "NEED_RESYNC", tag=tag,
    last_updated=as.character(timeStamp)))
  
  df[[1]] > 0
}

time.in.millis <- function() as.numeric(Sys.time()) * 1000
