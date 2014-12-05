#' funzione per salvare un grafo
#'
#' @name .saveGraph
#' @usage .saveGraph(x, tag)
#' @usage .saveGraph(x)
#' @include conflicts.r
#' @rdname saveGraph-internal

.saveGraph <- function(x, tag = x@tag) {
  if(hasConflicts(x)) {
    stop("Il grafo ha conflitti, risolverli prima di salvare")
  }
  if(tag == x@tag || .tagExists(tag)) {
    x@tag <- tag
    .updateGraph(x)
  } else {
    .createGraph(x, tag)
  }
}

.updateGraph <- function(x) {
  data <- x@data
  functions <- x@functions
  metadati <- x@metadati
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  dbSendQuery(con, "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE")
  dbBegin(con)
  tryCatch({

  }, error = function(err) {
    dbRollback(con)
    stop(err)
  })

  dbCommit(con)
}


#' crea ex-novo un istanza di grafo nel databae
#'
#' @name .createGraph
#' @rdname createGraph-internal
#' @param x istanza di Grafo
#' @param tag identificativo della versione
#' @usage .createGraph(g, tag)
#' @import plyr

.createGraph <- function(x, tag) {
  commento <- if(interactive()) {
    readline(prompt="Inserisci un commento/nota per: ")
  } else {
    paste0("Rilascio per ", tag)
  }
  
  con <- pgConnect()
  on.exit(dbDisconnect(con))

  dbSendQuery(con, "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE")
  dbBegin(con)
  tryCatch({
    dbGetPreparedQuery(
      con,
      paste0("insert into grafi(tag, commento, last_updated, autore) values ",
             "(?, ?, LOCALTIMESTAMP, ?)"),
      bind.data = data.frame(tag=tag, commento=commento, autore=whoami()))
    
    dati <- foreach (name = iter(names(x)), .combine=rbind) %dopar% {
      tt <- x[[name]]
      df <- to.data.frame(tt, name)
      cbind(tag, df)
    }
    dbGetPreparedQuery(
      con,
      paste0("insert into dati(tag, name, anno, periodo, freq, dati) values ",
             "(?, ?, ?, ?, ?, ?)"),
      bind.data = dati)

    archi <- as.data.frame(get.edgelist(x@network))
    archi <- cbind(tag, archi)
    names(archi) <- c('tag', 'partenza', 'arrivo')

    dbGetPreparedQuery(
      con,
      paste0("insert into archi(tag, partenza, arrivo) values ",
             "(?, ?, ?)"),
      bind.data = archi)
    
    metadati <- foreach(name = iter(names(x)), .combine=rbind) %dopar% {
      meta <- getMetadata(x, name)
      task <- getTask(x, name)
      if(!is.null(task)) {
        rbind(meta, data.frame(name=name, key="FORMULA", value=task))
      } else {        
        meta
      }
    }

    metadati <- cbind(tag, metadati)
    
    dbGetPreparedQuery(
      con,
      paste0("insert into metadati(tag, name, key, value) values ",
             "(?, ?, ?, ?)"),
      bind.data = metadati)
    
  }, error = function(err) {
    dbRollback(con)
    stop(err)
  })

  dbCommit(con)
}
