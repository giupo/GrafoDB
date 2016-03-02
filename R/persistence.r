
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
#' @include conflicts.r copy_graph.r
#' @rdname saveGraph-internal

# FIXME: #31849
# https://osiride-public.utenze.bankit.it/group/894smf/trac/cfin/ticket/31849
.saveGraph <- function(x, tag = x@tag, ...) {

  checkConflicts(x)
  if(hasConflicts(x)) {
    stop("Il grafo ",tag, " ha conflitti, risolverli prima di salvare")
  }

  param_list <- list(...)

  msg <- if('msg' %in% names(param_list)) {
    param_list[["msg"]]
  } else {
    NULL
  }
  
  tagExists <- .tagExists(tag)
  
  if(tagExists) {
    .updateGraph(x)
  } else {
    if (x@tag == tag) {
      .createGraph(x, tag, msg=msg)  
    } else {
      con <- pgConnect()
      on.exit(dbDisconnect(con))
      tryCatch({
        dbBegin(con)
        .copyGraph(x@tag, tag, con, msg=msg)
        .updateGraph(x, tag, con, msg=msg)
        dbCommit(con)
      }, error=function(cond) {
        dbRollback(con)
        stop(cond)
      })
    }
  }
}

#' @include update_archi.r update_data.r update_functions.r

.updateGraph <- function(x, tag=x@tag, con=NULL, ...) {
  param_list <- list(...)
  msg <- if('msg' %in% names(param_list)) {
    param_list[["msg"]]
  } else {
    NULL
  }
  wasNull <- is.null(con)
  if(wasNull) {
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    tryCatch({
      dbBegin(con)
    }, error = function(cond) {
      dbRollback(con)
      stop(cond)
    })
  }
  ## supporto per history
  tryCatch(
    doHistory(x, con),
    error = function(err) {
      dbRollback(con)
      stop(err)
    })
  
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
             " from (select last_updated as last_updated from dati ",
             " where tag=? ",
             " union select last_updated as last_updated from formule ",
             " where tag=? ",
             " union select last_updated as last_updated from archi ",
             " where tag=?)",
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

#' crea ex-novo un istanza di grafo nel databae
#'
#' @name .createGraph
#' @rdname createGraph-internal
#' @param x istanza di Grafo
#' @param tag identificativo della versione
#' @usage .createGraph(g, tag)
#' @importFrom RPostgreSQL2 dbGetPreparedQuery
#' @importFrom foreach foreach %do%
#' @importFrom rutils whoami
#' @importFrom RPostgreSQL2 dbBegin
#' @importFrom DBI dbSendQuery dbRollback

.createGraph <- function(x, tag, con=NULL, ...) {
  param_list <- list(...)
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
      paste0("insert into ",
             "dati(tag, name, anno, periodo, freq,",
             "dati, autore, last_updated) values ",
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
        paste0("insert into ",
               "archi(tag, partenza, arrivo, autore, last_updated) values ",
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
      data.frame(tag=character(0),
                 name=character(0),
                 task=character(0),
                 autore=character(0))
    }
  }

  if(length(nrow(formule))) {
    tryCatch({
      dbGetPreparedQuery(
        con,
        paste0("insert into ",
               "formule(tag, name, formula, autore, last_updated) values ",
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
#' @importFrom DBI dbGetQuery
#' @include db.r

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

#' Esegue il rolling dei vintage del `GrafoDB`
#'
#' Ad ogni salvataggio con il metodo `saveGraph` se non impostiamo un nuovo `tag`
#' il `GrafoDB` salva i dati sullo stesso `tag` ma contemporaneamente salva la versione
#' precedente con un progressivo, in modo da tener traccia di chi ha fatto cosa nel tempo.
#'
#' Le versioni sono contraddistinte da un nuovo tag, `tag`p`X` dove `X` e' un numero progressivo
#'
#' Il grafo potra' successivamente essere caricato con il nuovo tag.
#'
#' @name doHistory
#' @usage doHistory(x, con)
#' @param x istanza di `GrafoDB`
#' @param con connessione al database
#' @note questa e' una funzione interna del grafo invocata da `updateGraph`
#' @seealso saveGraph updateGraph
#' @importFrom DBI dbGetQuery
#' @importFrom RPostgreSQL2 dbGetPreparedQuery
#' @importFrom rprogressbar ProgressBar updateProgressBar kill
#' @importFrom RJSONIO toJSON
#' @importFrom iterators iter
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom rutils slice

doHistory <- function(x, con) {
  tag <- x@tag
  data <- x@data
  df <- dbGetQuery(con, paste0("select name from dati where tag = '",tag,"'"))
  if(nrow(df)) {
    nomi.db <- df$name
  } else {
    ## non faccio niente, non c'e' nulla sul DB
    return(invisible(NULL))
  }
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

  cl <- initCluster()
  is.multi.process <- !is.null(cl)

  if(is.multi.process) {
    if(wasWorking()) {
      stopCluster(cl)
      cl <- initCluster()
    } else {
      clusterStartWorking()
    }
  }
  
  lista.nomi <- slice(nomi.history, detectCores())
  
  blinda <- function(x, name, ordinale) {
   tag <- x@tag
    archi <- deps(x, name)
    if(is.null(archi)) {
      dbGetPreparedQuery(
        con,
        paste0(
          "insert into history(name, tag, ordinale, ",
          " anno, periodo, freq, dati,  last_updated, autore)",
          " select name, tag, ", ordinale,", anno, periodo, freq, dati, ",
          " last_updated, autore from dati_", tag," where name=?"),
        bind.data = data.frame(name))      
    } else {
      archi <- toJSON(archi)
      dbGetPreparedQuery(
        con,
        paste0(
          "insert into history(name, tag, ordinale, anno, ",
          " periodo, freq, dati, formula, archi_entranti, ",
          " last_updated, autore) ",
          "select d.name, d.tag, ?, d.anno, d.periodo, d.freq, ", 
          "d.dati, f.formula, ?, f.last_updated, f.autore ",
          " from dati_", tag," d, formule_", tag," f where f.tag = d.tag and d.tag=? ",
          " and d.name = f.name and d.name=?"), ## aggiunti i tag per evitare deadlock
        bind.data = data.frame(ordinale, archi, tag, name))      
    }
    name
  }
  
  combine <- function(pb) {
    count <- 0
    function(...) {
      x <- list(...)
      count <<- count + length(x)
      updateProgressBar(pb, count, last(unlist(x)))
      c(...)
    }
  }
  
  pb <- ProgressBar(min=1, max=length(nomi.history))
  
  if(is.multi.process && FALSE) {
    foreach(name = iter(nomi.history), .combine = combine(pb)) %dopar% {
      blinda(x, name, ordinale)      
    }
  } else {
    foreach(name = iter(nomi.history), .combine = combine(pb)) %do% {
      blinda(x, name, ordinale)
    }
  }
  kill(pb)
  doneWithCluster()
  message("Rolling history completo (", paste0(tag, "p", ordinale), ")")
}

#' Salva un istanza di grafo sul file system
#'
#' @name saveBinary
#' @usage saveBinary(x, path)
#' @param x istanza del GrafoDB
#' @param path percorso del file su cui salvare il grafo
#' @export

saveBinary <- function(x, path) {
  con <- file(path, "wb")
  on.exit(close(con))
  
  ret <- serialize(x, con, ascii = TRUE)
  invisible(ret)
}


#' Legge un GrafoDB dal filesystem in formato binario
#'
#' @name readBinary
#' @usage readBinary(path)
#' @param path percorso del file con il GrafoDB
#' @return GrafoDB contenuto nel file `path`
#' @export

readBinary <- function(path) {
  con <- file(path, "rw")
  on.exit(close(con))
  unserialize(con)
}

