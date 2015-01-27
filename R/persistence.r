
#' get all tags
#'
#' @name .tags
#' @rdname tags-internal
#' @return all the tags

.tags <- function() {
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  ret <- dbGetQuery(con, "select tag from grafi")
  as.character(ret$tag)
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

  tagExists <- .tagExists(tag)
  
  if(tagExists) {
    .updateGraph(x)
  } else {
    if (x@tag == tag) {
      .createGraph(x, tag)  
    } else {
      con <- pgConnect()
      on.exit(dbDisconnect(con))
      dbSendQuery(con, "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE")
      dbBegin(con)
      .copyGraph(x@tag, tag, con)
      .updateGraph(x, con, tag)
      dbCommit(con)
    }
  }
}

.copyGraph <- function(from, to, con=NULL) {
  if(is.null(con)) {
    wasNull <- TRUE
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    dbSendQuery(con, "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE")
    dbBegin(con)
  } else {
    wasNull <- FALSE
  }
  
  commento <- paste0("Rilascio per ", to)
  autore <- whoami()
  params <- cbind(to, autore, from)
  tryCatch({
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
      paste0("insert into grafi(tag, commento, last_updated, autore) values ",
             "(?, ?, LOCALTIMESTAMP::timestamp(0), ?)"),
      bind.data = data.frame(to, commento, autore))
    ## Ricordati di committare.
    if(wasNull) {
      dbCommit(con)
    }
  }, error = function(err) {
    dbRollback(con)
    stop(err)
  })
}

.updateGraph <- function(x, con=NULL, tag=x@tag) {
  if(is.null(con)) {
    wasNull <- TRUE
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    dbSendQuery(con, "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE")
    dbBegin(con)
  } else {
    wasNull <- FALSE
  }

  ## lo divido in tre blocchi tryCatch per finalita' di debugging.

  ## supporto per history
  doHistory(x, con)
  
  tryCatch(
    .updateData(x, con, tag),
    error = function(err) {
      dbRollback(con)
      stop(err)
    })

  tryCatch(
    .updateFunctions(x, con, tag),
    error = function(err) {
      dbRollback(con)
      stop(err)
    })

  tryCatch(
    .updateArchi(x, con, tag),
    error = function(err) {
      dbRollback(con)
      stop(err)
    })
  
  tryCatch({
    dbGetPreparedQuery(
      con,
      paste0(" update grafi set last_updated = (select max(last_updated) ",
             " from (select last_updated as last_updated from dati where tag=? ",
             " union select last_updated as last_updated from formule ",
             " where tag=? ",
             " union select last_updated as last_updated from archi where tag=?)",
             " as last_updated) where tag = ?"),
      bind.data = cbind(tag, tag, tag, tag))   
  }, error = function(err) {
    dbRollback(con)
    stop(err)
  })
  if(wasNull) {
    dbCommit(con)
  }
}

.updateArchi <- function(x, con, tag=x@tag) {
  data <- x@data
  functions <- x@functions
  timestamp <- x@timestamp
  network <- x@network
  in.memory <- as.data.frame(get.edgelist(network), stringsAsFactors = F)
  autore <- whoami()
  names(in.memory) <- c("partenza", "arrivo")
  in.db <- dbGetPreparedQuery(
    con,
    paste("select partenza, arrivo from archi where tag = ?"),
    bind.data = tag)
  sep <- "-"
  in.db <- drop.levels(in.db)
  in.db <- paste(in.db$partenza, in.db$arrivo, sep=sep)
  in.memory <- paste(in.memory$partenza, in.memory$arrivo, sep=sep)
  
  da.inserire <- setdiff(in.memory, in.db)

  df <- if(length(keys(data))) {
    ## cerco archi aggiunti di recente.
    params <- cbind(tag, timestamp)
    sql <- paste("select partenza, arrivo from archi where tag = ? ",
                 "and last_updated::timestamp(0) > to_timestamp(?)")
    dbGetPreparedQuery(con, sql, bind.data = params)
  } else {
    data.frame(partenza=character(0), arrivo=character(0))
  }
  
  if(nrow(df) > 0) {
    ## controllo che i nuovi archi non siano tra le serie che ho modificato e
    ## che non creino un anello
    wood <- graph.data.frame(df, directed=TRUE)
    network_aux <- graph.union(network, wood)
    if(any(keys(functions) %in% df$arrivo)) {
      warning("Ci sono conflitti sugli archi, continuo su dati e formule")    
    }
    
    if(!is.dag(network_aux)) {
      wrongsort <- try(topological.sort(network), silent=TRUE)
      network_seq <- V(network)
      cycles_seq <- network_seq[setdiff(
        network_seq, network_seq[wrongsort])]
      cycles_vertex <- cycles_seq$name
      stop("Cycles found: ", paste(unlist(cycles_vertex), collapse=", "))
    }
  }
  
  if(length(da.inserire)) {
    params <- if(length(da.inserire) == 1) {
      tokens <- str_split(da.inserire, sep)[[1]]
      df <- as.data.frame(
        list(
          partenza = tokens[[1]],
          arrivo=tokens[[2]]),
        stringsAsFactors = F)
      names(df) <- c("partenza", "arrivo")
      df
    } else {
      splitted <- unlist(str_split(da.inserire, sep))
      df <- as.data.frame(matrix(splitted, nrow=length(da.inserire), byrow=T), stringsAsFactors = F)
      names(df) <- c("partenza", "arrivo")
      df
    }
    params <- c(tag, df, autore)
    dbGetPreparedQuery(
      con,
      "insert into archi(tag, partenza, arrivo, autore) values(?,?,?,?)",
      bind.data = params)
  }
}
  
.updateData <- function(x, con, tag=x@tag) {
  data <- x@data
  timestamp <- x@timestamp
  autore <- whoami()
  df <- if(length(keys(data))) {
    params <- cbind(tag, timestamp)
    sql <- paste("select name from dati where tag = ? ",
                 "and last_updated::timestamp(0) > to_timestamp(?)")
    
    dbGetPreparedQuery(con, sql, bind.data = params)
  } else {
    data.frame()
  }
  
  names.with.conflicts <- intersect(keys(data), as.character(df$name))
  if(length(names.with.conflicts) > 0) {
    ## ci sono conflitti
    ## crea il conflitto e non toccare i dati.
    sql <- paste0("insert into conflitti(tag, name, anno, prd, ",
                  " freq, dati, autore)",
                  " values (?, ?, ?, ?, ?, ? ,?)")
    
    dati <- foreach(name = iter(names.with.conflicts),.combine=rbind) %do% {
      tryCatch({
        tt <- x[[name]]
        df <- to.data.frame(tt, name)
        cbind(tag, df, autore)
      }, error = function(err) {
        stop(name, ": ", err)
      })
    }
    
    dati <- as.data.frame(dati)
    names(dati) <- c("tag", names(df), "autore")
    dbGetPreparedQuery(con, sql, bind.data = dati)
    warning("Ci sono conflitti sui dati per le serie: ",
            paste(names.with.conflicts, collapse=", "))
  }
  ## aggiorno solo le serie cambiate
  
  names.updated <- setdiff(keys(data), names.with.conflicts)
  cl <- initCluster()
  is.multi.process <- !is.null(cl)
  autore <- whoami()
  if(length(names.updated)) { 
    dati <- if(is.multi.process) {
      foreach (name = iter(names.updated), .combine=rbind) %dopar% {
        df <- to.data.frame(data[[name]])
        cbind(df, autore, name, tag) 
      }
    } else {
      foreach (name = iter(names.updated), .combine=rbind) %do% {
        df <- to.data.frame(data[[name]])
        cbind(df, autore, name, tag)    
      }
    }
    

    if(dbExistsTable(con, paste0("dati_", tag))) {
      sql1 <- paste0("UPDATE dati_",tag,
                     " SET anno=?, periodo=?, freq=?, dati=?,",
                     "autore=?, last_updated = LOCALTIMESTAMP::timestamp(0) ",
                     " WHERE name=? and tag=?");
      dbGetPreparedQuery(con, sql1, bind.data=dati)
    }
    
    sql2 <- paste0(
      "INSERT INTO dati(anno, periodo, freq, dati, autore, ",
      " name, tag, last_updated) ",
      " select ?,?,?,?,?,?,?,LOCALTIMESTAMP::timestamp(0)",
      " WHERE NOT EXISTS (SELECT 1 FROM dati WHERE name=? and tag=?)")
    
    dati <- cbind(dati, names.updated, tag)
    dbGetPreparedQuery(con, sql2, bind.data=dati)
  }
}

.updateFunctions <- function(x, con, tag=x@tag) {
  ## passo la connessione perche' devono avere la stessa transazione
  ## non usare controllo di transazione qui
  functions <- x@functions
  timestamp <- x@timestamp
  df <- if(length(keys(functions))) {
    params <- cbind(tag, timestamp)
    sql <- paste("select name from formule where tag = ? ",
                 "and last_updated::timestamp(0) > to_timestamp(?)")
    dbGetPreparedQuery(con, sql, bind.data = params)
  } else {
    data.frame()
  }
  
  names.with.conflicts <- intersect(keys(functions), as.character(df$name))
  cl <- initCluster()
  is.multi.process <- !is.null(cl)
  autore <- whoami()
  if(length(names.with.conflicts)) {
    dati <- if(is.multi.process) {
      foreach (name = iter(names.with.conflicts), .combine=rbind) %dopar% {
        task <- expr(x, name, echo=FALSE)
        cbind(task, autore, name, tag)
      }
    } else {
      foreach (name = iter(names.with.conflicts), .combine=rbind) %do% {
        task <- expr(x, name, echo=FALSE)
        cbind(task, autore, name, tag)
      }
    }
    
    sql1 <- paste0("UPDATE conflitti  SET formula=?, autore=?, ",
                   "date = LOCALTIMESTAMP::timestamp(0) ",
                   " WHERE name=? and tag=?");      
    dbGetPreparedQuery(con, sql1, bind.data=dati)
    
  
    sql2 <- paste0(
      "INSERT INTO conflitti(formula, autore, date, name, tag) ",
      " select ?,?,LOCALTIMESTAMP::timestamp(0),?,?",
      " WHERE NOT EXISTS (SELECT 1 FROM formule WHERE name=? and tag=?)")
    dati <- cbind(dati, names.with.conflicts, tag)
    
    names(dati) <- c("formula", "autore", "name", "tag", "name", "tag")
    dbGetPreparedQuery(con, sql2, bind.data = dati)
    warning("Ci sono conflitti sulle formule per le serie: ",
            paste(names.with.conflicts, collapse=", "))
  } 
  
  names.updated <- setdiff(keys(x@functions), names.with.conflicts)
  if(length(names.updated)) {
    formule <- if(is.multi.process) {
      foreach (name = iter(names.updated), .combine=rbind) %dopar% {
        task <- expr(x, name, echo=FALSE)
        cbind(task, whoami(), name, tag)
      }
    } else {
      foreach (name = iter(names.updated), .combine=rbind) %do% {
        task <- expr(x, name, echo=FALSE)
        cbind(task, whoami(), name, tag)
      }
    }
    
    if(dbExistsTable(con, paste0("formule_", tag))) {
      sql1 <- paste0("UPDATE formule_",tag,
                     " SET formula=?, autore=?, ",
                     " last_updated = LOCALTIMESTAMP::timestamp(0) ",
                     " WHERE name=? and tag=?");      
      dbGetPreparedQuery(con, sql1, bind.data=formule)
    }
    
    sql2 <- paste0(
      "INSERT INTO formule(formula, autore, name, tag, last_updated) ",
      " select ?,?,?,?,LOCALTIMESTAMP::timestamp(0) ",
      " WHERE NOT EXISTS (SELECT 1 FROM formule WHERE name=? and tag=?)")
    autore <- whoami()
    formule <- foreach (name = iter(names.updated), .combine=rbind) %do% {
      task <- expr(x, name, echo=FALSE)
      cbind(task, autore, name, tag, name, tag)
    }
    colnames(formule) <- c("formula", "autore", "name", "tag", "name", "tag")
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

.createGraph <- function(x, tag, con=NULL) {
  if(is.null(con)) {
    wasNull <- TRUE
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    dbSendQuery(con, "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE")
    dbBegin(con)
  } else {
    wasNull <- FALSE
  }
  
  commento <- if(interactive()) {
    ## readline(prompt="Inserisci un commento/nota per: ")
    "BATMAN"
  } else {
    paste0("Rilascio per ", tag)
  }
  commento = "Batman"
  autore <- whoami()
  tryCatch({
    dbGetPreparedQuery(
      con,
      paste0("insert into grafi(tag, commento, last_updated, autore) values ",
             "(?, ?, LOCALTIMESTAMP::timestamp(0), ?)"),
      bind.data = data.frame(tag=tag, commento=commento, autore=autore))
  }, error = function(err) {
    dbRollback(con)
    stop(err)
  })
  
  if(length(names(x))) {
    dati <- foreach (name = iter(names(x)), .combine=rbind) %do% {
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
      paste0("insert into dati(tag, name, anno, periodo, freq, dati, autore, last_updated) values ",
             "(?, ?, ?, ?, ?, ?, ?, LOCALTIMESTAMP::timestamp(0))"),
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
        paste0("insert into archi(tag, partenza, arrivo, autore, last_updated) values ",
               "(?, ?, ?, ?, LOCALTIMESTAMP::timestamp(0))"),
        bind.data = archi)
    }, error = function(err) {
      dbRollback(con)
      stop(err)
    })
  }
  autore <- whoami()
  formule <- foreach(name = iter(names(x)), .combine=rbind) %do% {
    task <- expr(x, name, echo=F)
    if(!is.null(task)) {
      cbind(tag, name, task, autore)
    } else {
      data.frame(tag=character(0), name=character(0), task=character(0), autore=character(0))
    }
  }

  if(length(nrow(formule))) {
    tryCatch({
      dbGetPreparedQuery(
        con,
        paste0("insert into formule(tag, name, formula, autore, last_updated) values ",
               "(?, ?, ?, ?, LOCALTIMESTAMP::timestamp(0))"),
        bind.data = formule)
    
    }, error = function(err) {
      dbRollback(con)
      stop(err)
    })
  }
  if(wasNull) {
    dbCommit(con)
  }
}


#' conta le versioni rolling del grafo con tag `tag`
#'
#' @name countRolling
#' @usage countRolling(x)
#' @param x istanza di grafo
#' @param con connessione al DB
#' @return un intero ad indicare il numero di versioni rolling salvate sul DB

countRolling <- function(x, con = NULL) {
  if(is.null(con)) {
    con <- pgConnect()
    on.exit(dbDisconnect(con))
  }
  tag <- x@tag
  sql <- paste0("select count(tag) from grafi where tag like '", tag, "p%'")
  df <- dbGetQuery(con, sql)
  
  as.numeric(df[[1,1]])
}

#' Esegue il push delle release di un entita' sul DB
#'
#' @name pushTable
#' @usage pushTable(x, tag)
#' @param x entity name
#' @param tag tag di riferimento
#' @param con connessione su cui avviene la transazione
#' @note funzione interna per il versionamento

pushTable <- function(x, tag, con) {
  
}

doHistory <- function(x, con) {
  tag <- x@tag

  data <- x@data
  nomi.db <- names(x)
  nomi.data <- keys(data)
  nomi.history <- intersect(nomi.db, nomi.data)
  if(length(nomi.history)  == 0 ) {
    return()
  }
  message("Rolling history per ", tag)
  df <- dbGetPreparedQuery(
    con,
    "select max(ordinale) from history where tag=?",
    bind.data = tag)

  ordinale <- as.numeric(df[[1,1]])
  ordinale <- if(is.na(ordinale)) {
    1
  } else {
    ordinale + 1
  }
  pb <- ProgressBar(0, length(nomi.history))
  i <- 0
  for(name in nomi.history) {
    archi <- deps(x, name)
    i <- i + 1
    update(pb, i, label=name)
    if(is.null(archi)) {
      dbGetPreparedQuery(
        con,
        paste0("insert into history(name, tag, ordinale, anno, periodo, freq, dati, formula, archi_entranti, last_updated, autore)",
               "select name, tag, ?, anno, periodo, freq, dati, last_updated, autore from dati where tag=? and name=?"),
        bind.data = data.frame(ordinale, tag, name))      
    } else {
      archi <- toJSON(archi)
      dbGetPreparedQuery(
        con,
        paste0("insert into history(name, tag, ordinale, anno, periodo, freq, dati, formula, archi_entranti, last_updated, autore)",
               "select d.name, d.tag, ?, d.anno, d.periodo, d.freq, d.dati, f.formula, ?, f.last_updated, f.autore from dati d, formule f where f.tag = d.tag and d.tag=? and d.name = f.name and d.name=?"),
        bind.data = data.frame(ordinale, archi, tag, name))      
    }
  }
  kill(pb)
  message("Rolling history completo (", paste0(tag, "p", ordinale), ")")
}
