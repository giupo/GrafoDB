
#' get all tags
#'
#' @name .tags
#' @rdname tags-internal
#' @return all the tags

.tags <- function() {
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  ret <- dbGetQuery(con, "select tag from grafi")
  as.character(ret$tags)
}

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
  if(.tagExists(tag)) {
    x@tag <- tag
    .updateGraph(x)
  } else {
    .createGraph(x, tag)
  }
}

.updateGraph <- function(x) {
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  dbSendQuery(con, "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE")
  dbBegin(con)
  tryCatch(
    .updateData(x, con),
    error = function(err) {
      dbRollback(con)
      stop(err)
    })
  
  tryCatch(
    .updateFunctions(x, con),
    error = function(err) {
      dbRollback(con)
      stop(err)
    })
  
  dbCommit(con)
}

.updateData <- function(x, con) {
  tag <- x@tag
  data <- x@data
  timestamp <- x@timestamp
  df <- if(length(keys(data))) {
    params <- cbind(tag, keys(data), timestamp)
    sql <- "select name from dati where tag = ? and name = ? and last_updated > to_timestamp(?)"
    dbGetPreparedQuery(con, sql, bind.data = params)
  } else {
    data.frame()
  }
  
  if(nrow(df) > 0) {
    ## ci sono conflitti
    ## crea il conflitto e non toccare i dati.
    names.with.conflicts <- as.character(df$name)
    sql <- paste0("insert into conflitti(tag, name, anno, prd, freq, dati, autore)",
                  "values (?, ?, ?, ?, ?, ? ,?)")
    
    dati <- foreach (name = iter(names.with.conflicts), .combine=rbind) %dopar% {
      tt <- x[[name]]
      df <- to.data.frame(tt, name)
      cbind(tag, df, whoami())
    }
    dati <- as.data.frame(dati)
    names(dati) <- c("tag", names(df), "autore")
    dbGetPreparedQuery(con, sql, bind.data = dati)
    warning("Ci sono conflitti sui dati per le serie: ",
            paste(names.with.conflicts, collapse=", "))
  }
  ## aggiorno solo le serie cambiate
  
  names.updated <- setdiff(keys(x@data), as.character(df$name))
  if(length(names.updated)) {
    dati <- foreach (name = iter(names.updated), .combine=rbind) %dopar% {
      df <- to.data.frame(x[[name]])
      autore <- whoami()
      cbind(df, autore, name, tag)
    }
    ## dati <- cbind(dati, whoami(), names.updated, tag)
    if(dbExistsTable(con, paste0("dati_", tag))) {
      sql1 <- paste0("UPDATE dati_",tag,
                     " SET anno=?, periodo=?, freq=?, dati=?, autore=? WHERE name=? and tag=?");
      dbGetPreparedQuery(con, sql1, bind.data=dati)
    }
    sql2 <- paste0(
      "INSERT INTO dati(anno, periodo, freq, dati, autore, name, tag) select ?,?,?,?,?,?,? ",
      " WHERE NOT EXISTS (SELECT 1 FROM dati WHERE name=? and tag=?)")
    
    dati <- cbind(dati, names.updated, tag)
    dbGetPreparedQuery(con, sql2, bind.data=dati)
  }
}

.updateFunctions <- function(x, con) {
  ## passo la connessione perche' devono avere la stessa transazione
  ## non usare controllo di transazione qui
  tag <- x@tag
  functions <- x@functions
  df <- if(length(keys(functions))) {
    params <- cbind(tag, keys(functions), x@timestamp)
    sql <- paste("select name from formule where tag = ? and name = ? ",
                 "and last_updated > to_timestamp(?)")
    dbGetPreparedQuery(con, sql, bind.data = params)
  } else {
    data.frame()
  }
  
  if(nrow(df)) {
    names.with.conflicts <- as.character(df$name)
    sql <- paste0("insert into conflitti(tag, name, formula, autore)",
                  "values (?, ?, ?, ?)")
    
    dati <- foreach (name = iter(names.with.conflicts), .combine=rbind) %dopar% {
      getTask(x, name)
    }
    dati <- cbind(tag, dati, whoami())
    
    dbGetPreparedQuery(con, sql, bind.data = dati)
    warning("Ci sono conflitti sulle formule per le serie: ",
            paste(names.with.conflicts, collapse=", "))
  } 
  
  
  names.updated <- setdiff(keys(x@functions), as.character(df$name))
  if(length(names.updated)) {
    formule <- foreach (name = iter(names.updated), .combine=rbind) %dopar% {
      task <- getTask(x, name)
      cbind(task, whoami(), name, tag)
    }

    if(dbExistsTable(con, paste0("formule_", tag))) {
      sql1 <- paste0("UPDATE formule_",tag,
                     " SET formula=?, autore=? WHERE name=? and tag=?");      
      dbGetPreparedQuery(con, sql1, bind.data=formule)
    }
    
    sql2 <- paste0(
      "INSERT INTO formule(formula, autore, name, tag) select ?,?,?,? ",
      " WHERE NOT EXISTS (SELECT 1 FROM formule WHERE name=? and tag=?)")

    formule <- t(as.data.frame(getTask(x, names.updated), stringAsFactors=F))
    autore <- whoami()
    formule <- cbind(formule, autore, rownames(formule), tag, rownames(formule), tag)
    dbGetPreparedQuery(con, sql2, bind.data=formule)
  }
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
  }, error = function(err) {
    dbRollback(con)
    stop(err)
  })
  
  if(length(names(x))) {
    dati <- foreach (name = iter(names(x)), .combine=rbind) %dopar% {
      tt <- x[[name]]
      df <- to.data.frame(tt, name)
      autore <- whoami()
      cbind(tag, df, autore)
    }
  } else {
    stop("Non ci sono dati da salvare.")
  }

  tryCatch({
    dbGetPreparedQuery(
      con,
      paste0("insert into dati(tag, name, anno, periodo, freq, dati, autore) values ",
             "(?, ?, ?, ?, ?, ?, ?)"),
      bind.data = dati)
  }, error = function(err) {
    dbRollback(con)
    stop(err)
  })
  
  archi <- as.data.frame(get.edgelist(x@network))
  autore <- whoami()
  
  if(nrow(archi)) {
    archi <- cbind(tag, archi, autore)
    names(archi) <- c('tag', 'partenza', 'arrivo', 'autore')
    tryCatch({
      dbGetPreparedQuery(
        con,
        paste0("insert into archi(tag, partenza, arrivo, autore) values ",
               "(?, ?, ?, ?)"),
        bind.data = archi)
    }, error = function(err) {
      dbRollback(con)
      stop(err)
    })
  }
  
  formule <- foreach(name = iter(names(x)), .combine=rbind) %do% {
    task <- getTask(x, name)
    if(!is.null(task)) {
      autore <- whoami()
      cbind(tag, name, task, autore)
    } else {
      data.frame(tag=character(0), name=character(0), task=character(0), autore=character(0))
    }
  }

  if(length(nrow(formule))) {
    tryCatch({
      dbGetPreparedQuery(
        con,
        paste0("insert into formule(tag, name, formula, autore) values ",
               "(?, ?, ?, ?)"),
        bind.data = formule)
    
    }, error = function(err) {
      dbRollback(con)
      stop(err)
    })
  }
  dbCommit(con)
}
