#' Funzione d'inizializzazione del grafo.
#'
#' Questa funzione va utilizzata nell'initialize S4 dell'oggetto `GrafoDB`
#'
#' @name .init
#' @rdname init-internal
#' @param .Object (creato da new)
#' @param tag tag del grafo (default=`cf10`)
#' @return un istanza di grafo popolata correttamente secono i parametri (`tag`)
#' @note e' stata scorporata dall'initialize S4 per finalita' di debug
#' @include persistence.r RcppExports.R
#' @import stringr

.init <- function(.Object, tag="cf10") {
  if(is.null(tag)) {
    tag <- "cf10"
  }

  .Object@edges <- hash()
  .Object@data <- hash()
  .Object@functions <- hash()
  .Object@touched <- character(0)
  .Object@ordinal <- if(grepl("p(\\d+)$", tag)) {
    mth = str_match(tag, "p(\\d+)$")
    tag <- gsub(paste0(mth[1,1], "$"), "", tag)
    as.numeric(mth[1,2])
  } else {
    0
  }
  .Object@tag <- tag
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  username <- whoami()
  password <- flypwd()
  settings <- dbSettings()
  network <- load_archi(username, password, settings$host,
                        settings$port, settings$dbname, tag);
  
  network <- if(nrow(network) > 0) {
    graph.data.frame(as.data.frame(network), directed=TRUE)
  } else {
    graph.empty(directed=TRUE)
  }
  
  df <- dbGetPreparedQuery(
    con,
    "select name from dati where tag=?",
    bind.data = tag)
  
  if(nrow(df)) {
    nomi <- as.character(df$name)
    pending.names <- setdiff(nomi, V(network)$name)
    network <- network + vertex(pending.names)
  }
  
  .Object@network <- network
  
  df <- dbGetPreparedQuery(
    con,
    "select * from grafi where tag = ?",
    bind.data = tag)

  
  if(nrow(df)) {
    ## il grafo esiste nel DB
    .Object@timestamp <- df$last_updated
    if(interactive()) {
      message(df$comment)
    }
  } else {
    ## il grafo non esiste nel DB
    .Object@timestamp <- Sys.time()
    sql <- paste0(
      "INSERT INTO grafi(tag, commento, last_updated, autore) ",
      " select ?,?,LOCALTIMESTAMP::timestamp(0),? ",
      " WHERE NOT EXISTS (SELECT 1 FROM grafi WHERE tag=?)")
    autore <- whoami()
    dati <- cbind(tag, paste0('Grafo per ', tag), autore, tag)
    names(dati) <- c("tag", "commento", "autore", "tag")
    dbGetPreparedQuery(con, sql, bind.data = dati)
  }
  
  .Object
}


#' Controlla se `x` e' un `GrafoDB`
#'
#' Predicato; Ritorna `TRUE` se `x` e' un istanza di `GrafoDB`, altrimenti ritorna
#' `FALSE`
#'
#' @name is.grafodb
#' @usage is.grafodb(x)
#' @param x un qualsiasi oggetto `R`
#' @return `TRUE` se `x` e' un `GrafoDB`, altrimenti `FALSE`
#' @examples \dontrun{
#'   g = GrafoDB()
#'   is.grafodb(g) # questo e' TRUE
#'   x = list()
#'   is.grafodb(x) # questo e' FALSE
#' }
#' @export

is.grafodb <- function(x) {
  inherits(x, "GrafoDB")
}

#' converte una timeseries `BIMETS`
#' o un generico scalare in un data.frame.
#' funzione utilizzata per convertire il dato in una forma accettabile dal DB
#'
#' @name to.data.frame
#' @usage to.data.frame(x)
#' @param x una timeseries `BIMETS` o uno scalare
#' @param name nome da dare alla timeseries
#' @return una rappresentazione a data.frame della serie `x`
#' @note funzione interna
#' @rdname todataframe

to.data.frame <- function(x, name=NULL) {
  ## questa funzione converte a dataframe la timeseries,
  ## utile per l'inserimento nel DB
  if(is.bimets(x)) {
    anno <- TSINFO(x, MODE="STARTY")
    prd  <- TSINFO(x, MODE="STARTP")
    freq <- TSINFO(x, MODE="FREQ")
  } else {
    anno <- 0
    prd <- 0
    freq <- 0
  }
  raw_numbers <- gsub(" ", "", toJSON(x, digits=20))

  if(is.null(name)) {
    as.data.frame(
      list(anno=anno, periodo=prd,
           freq=freq, dati=raw_numbers), stringsAsFactors = F)
  } else {
    as.data.frame(
      list(name=name, anno=anno, periodo=prd,
           freq=freq, dati=raw_numbers), stringAsFactors = F)
  }
}

#' converte un dataframe (caricato dal Database) in una timeseries `BIMETS`
#'
#' @name from.data.frame
#' @usage from.data.frame(df)
#' @param df data.frame compilato dal database
#' @import RJSONIO
#' @note i dati dal db sono memorizzati come stringhe JSON
#' @rdname fromdataframe

from.data.frame <- function(df) {
  stopifnot(is.data.frame(df))
  ret <- list()

  for(i in seq(nrow(df))) {
    row <- df[i,]
    anno <- row$anno
    periodo <- row$periodo
    freq <- row$freq
    params <- c(anno, periodo, freq)
    if(any(params == 0)) {
      ret[[row$name]] <- fromJSON(as.character(row$dati))
    } else {
      dati <- TSERIES(
        fromJSON(as.character(row$dati), nullValue = NA),
        START=c(anno, periodo),
        FREQ=freq)
      ret[[row$name]] <- dati
    }
  }
  ret
}


#' funzione per eliminare le definizione 'function' dalle formule per il GrafoDB
#'
#' @name .declutter_function
#' @usage .declutter_function(f)
#' @param f formula in formato testo
#' @import stringr
#' @rdname declutter-function-internal

.declutter_function <- function(f) {
  f <- if(is.function(f)) {
    f <- paste(deparse(f), collapse="\n")
  } else {
    f
  }
  idx <- str_locate(f, "\\{")
  f <- substring(f, idx[[1]] + 1)
  f <- gsub("\\}$", "", f)
  f <- gsub("^\n(.*)\n$", "\\1", f)
  str_trim(f)
}

#' Questa funzione orla le funzioni del grafo con `proxy <-function() {` e `}` finale.
#'
#' Le istruzioni vengono incapsulate in una funzione generica chiamata proxy.
#' gli argomenti devono essere definiti prima nella ambiente per la corretta esecuzione
#'
#' @name .clutter_function
#' @usage .clutter_function(f)
#' @param f character array che rappresenta la funzione
#' @param name name of the object to be returned
#' @param funcName name of the function (`proxy` default)
#' @return un character array della funzione orlata
#' @note funzione interna
#' @rdname clutter_function

.clutter_function <- function(f, name, funcName="proxy") {
  template <- "--FUNCNAME-- <- function() {
  --FUNCTION--
  --NAME--
}"
  task <- gsub("--FUNCNAME--", funcName, template)
  task <- gsub("--FUNCTION--", f, task)
  task <- gsub("--NAME--", name, task)
  task
}


#' questa funzione orla la formula del grafo come una funzione
#'
#' I parametri della funzione ritornata sono le dipendenze della serie
#' @name .clutter_with_params
#' @usage .clutter_with_params(f, name, deps)
#' @param f function task to be converted as function
#' @param name task name
#' @param deps character array di dipendenze
#' @return Ritorna una una funzione `is.character(ret) == TRUE`
#' @rdname clutter_with_params_internal

.clutter_with_params <- function(f, name, deps) {
  template <- "proxy <- function(--DEPS--) {
  --FUNCTION--
}"
  task <- gsub("--DEPS--", paste(deps, collapse = ", "), template)
  task <- gsub("--FUNCTION--", f, task)
  task
}

#' questa funzione orla la formula del grafo come una funzione
#'
#' I parametri della funzione ritornata sono le dipendenze della serie,
#' ed aggiunge il nome della funzione al termine per dichiarare il
#' dato ritornato
#' 
#' @name .clutter_with_params_and_return
#' @usage .clutter_with_params_and_return(f, name, deps, funcName)
#' @param f function task to be converted as function
#' @param name task name
#' @param deps array di dipendenze
#' @param funcName nome della funzione definita
#' @return Ritorna un character array `is.character(ret) == TRUE`
#' @rdname clutter_with_params_and_return_internal

.clutter_with_params_and_return <- function(f, name, deps, funcName="proxy") {
  template <- "--FUNCNAME-- <- function(--DEPS--) {
  --FUNCTION--
  --NAME--
}"

  task <- gsub("--DEPS--", paste(deps, collapse=", "), template)
  task <- gsub("--FUNCTION--", f, task)
  task <- gsub("--FUNCNAME--", funcName, task)
  task <- gsub("--NAME--", name, task)
  task
}


#' Ritorna le formule del GrafoDB
#'
#' Ritorna (o mostra su console, stdout) le formule delle serie `nomi`
#'
#' @name  .expr
#' @usage .expr(x, name)
#' @usage .expr(x, name, echo=FALSE)
#' @param x istanza di GrafoDB
#' @param nomi array di nomi di serie storiche
#' @rdname expr-internal
#' @import formatR

.expr <- function(x, nomi, echo=FALSE) {
  functions <- x@functions
  in.functions <- intersect(keys(functions), nomi)
  da.caricare.db <- setdiff(nomi, in.functions)
  from.db <- if(length(da.caricare.db)) {
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    params <- as.data.frame(cbind(x@tag, da.caricare.db))
    names(params) <- c("tag", "name")
    dbGetPreparedQuery(
      con,
      "select name, formula from formule where tag = ? and name = ?",
      bind.data = params)
  } else {
    data.frame(name=character(), formula=character())
  }
  
  
  in.functions <- foreach(
    row=iter(in.functions, by='row'),
    .combine=rbind) %do% {
      data.frame(name=row, formula=functions[[row]])
    }
  
  formule <- rbind(in.functions, from.db)
  
  if(nrow(formule) == 0) {
    NULL
  } else if(nrow(formule) == 1) {
    task <- as.character(formule$formula)
    if(interactive() && echo) {
      tidy_source(text=task, indent= 2)
    }
    invisible(task)
  } else {
    nomi <- formule$name
    ret <- vector(length(nomi), mode="list")
    for(i in seq_along(nomi)) {
      name <- nomi[[i]]
      ret[i] <- as.character(formule[formule$name == name,]$formula)
    }
    names(ret) <- nomi
    ret
  }
}

.evaluateSingle1 <- function(name, graph) {
  tsformula <- .expr(graph, name, echo=FALSE)
  nomi_padri <- upgrf(graph, name, livello=1)
  if(length(nomi_padri) == 0 && is.null(tsformula)) {
    return(graph[[name]])
  }
  if(length(nomi_padri) > 100) {
    padri <- list()
    sliced <- slice(nomi_padri, n=100)
    foreach(sliced_names = sliced) %do% {
      padri[sliced_names] <- graph[[sliced_names]]
    }
    names(padri) <- nomi_padri
    padri
  } else if (length(nomi_padri)) {
    padri <- graph[[nomi_padri]]
  } else {
    padri <- list()
  }
  
  if(length(nomi_padri) == 1) {
    ## boxing
    ppp <- list()
    ppp[[nomi_padri]] <- padri
    padri <- ppp
  }
  ## cmd <- .clutter_function(tsformula, name)
  cmd <- .clutter_with_params_and_return(tsformula, name, nomi_padri)
  tryCatch({    
    eval(parse(text=cmd))
    do.call(proxy, padri)    
  }, error = function(err) {
    stop(name, ": ", err)
  })           
}


.evaluateSingle3 <- function(name, graph) {
  tsformula <- .expr(graph, name, echo=FALSE)
  nomi_padri <- upgrf(graph, name, livello=1)
  if(length(nomi_padri) == 0) {
    return(graph[[name]])
  }
  if(length(nomi_padri) > 100) {
    padri <- list()
    sliced <- slice(nomi_padri, n=100)
    foreach(sliced_names = sliced) %do% {
      padri[sliced_names] <- graph[[sliced_names]]
    }
    names(padri) <- nomi_padri
    padri
  } else {
    padri <- graph[[nomi_padri]]
  }
  
  if(length(nomi_padri) == 1) {
    ## boxing
    ppp <- list()
    ppp[[nomi_padri]] <- padri
    padri <- ppp
  }

  attach(padri)
  on.exit(detach(padri))
  cmd <- .clutter_function(tsformula, name)
  tryCatch({
    eval(parse(text=cmd))
    proxy()
  }, error = function(err) {
    stop(name, ": ", err)
  })           
}

.evaluateSingle2 <- function(name, graph) {
  tsformula <- .expr(graph, name, echo=FALSE)
  nomi_padri <- upgrf(graph, name, livello=1)
  if(length(nomi_padri) == 0) {
    return(graph[[name]])
  }
  if(length(nomi_padri) > 100) {
    padri <- list()
    sliced <- slice(nomi_padri, n=100)
    foreach(sliced_names = sliced) %do% {
      padri[sliced_names] <- graph[[sliced_names]]
    }
    names(padri) <- nomi_padri
    padri
  } else {
    padri <- graph[[nomi_padri]]
  }
  
  if(length(nomi_padri) == 1) {
    ## boxing
    ppp <- list()
    ppp[[nomi_padri]] <- padri
    padri <- ppp
  }
  env <- as.environment(padri)
  env$graph <- graph
  cmd <- .clutter_function(tsformula, name)
  tryCatch({
    eval(parse(text=cmd))
    attach(env)
    do.call(proxy, list())
  }, error = function(err) {
    stop(name, ": ", err)
  })           
}

#' Valuta un singolo oggetto del grafo identificato da `name`
#'
#' @name .evaluateSingle
#' @aliases evaluateSingle
#' @usage .evaluateSingle(name, object)
#' @usage evaluateSingle(name, object)
#' @param name `character` nome della serie
#' @param graph istanza di `GrafoDB`
#' @return la serie storica calcolata.
#' @export
#' @import grafo
#' @rdname evaluateSingle-internal
#' @note la scelta di evaluateSingle ricade su .evaluateSingle1, le altre due
#'       implementazioni NON SUPPORTANO LE SERIE ELEMENTARI

.evaluateSingle <- .evaluateSingle1

#' Implementazione del generic `evaluate` definito nel package `grafo`
#' per la classe `GrafoDB`
#'
#' Questa funzione valuta i nodi del grafo in parallelo
#'
#' @name .evaluate
#' @usage .evaluate(object)
#' @usage .evaluate(object, v_start)
#' @return il grafo con i dati correttamente valutati
#' @import grafo igraph rcf
#' @rdname evaluate-internal

.evaluate <- function(object, v_start=NULL, deep=T, ...) {
  params <- list(...)
  debug <- if("debug" %in% names(params)) {
    as.logical(params[["debug"]])
  } else {
    FALSE
  }
  tag <- object@tag
  PRELOAD_TAG <- "preload"
  data <- object@data
  network <- object@network
  all_names <- names(object)
  
  if(!all(v_start %in% all_names)) {
    not.in.graph <- setdiff(v_start, all_names)
    stop("Non sono serie del grafo: ", paste(not.in.graph, collapse=", "))
  }

  if(is.null(v_start)) {
    ## le voglio valutare tutte
    if(!tag %in% c(PRELOAD_TAG, "biss", "pne", "dbcong")) {
      sources_id <- V(network)[degree(network, mode="in") == 0]
      nomi_sources <- V(network)[sources_id]$name
      sources <- getdb(nomi_sources, PRELOAD_TAG)
      if(is.bimets(sources)) {
        data[preload_V_start] <- sources
      } else {
        data[names(sources)] <- sources
      }
      ## fonti gia' valutate, le tolgo
      network <- delete.vertices(network, sources_id)
    } 
  } else {
    v_start <- as.character(v_start)
    network <- induced.subgraph(
      network,
      V(network)[unlist(
        neighborhood(network, order=.Machine$integer.max, nodes=v_start, mode="out")
        )])
  }
  
  ## se il network e' vuoto dopo l'eliminazione delle sorgenti,
  ## ritorno senza fare nulla
  if(!length(V(network))) {
    return(invisible(object))
  }
  
  deep <- as.logical(deep)
  
  total <- length(V(network))
  i <- 0
  is.interactive <- interactive()
  if(is.interactive) {
    pb <- ProgressBar(min=0, max=total)
    update(pb, i, "Starting...")
    update(pb, i, "Try Cluster...")
  }  
  
  cl <- initCluster()
  is.multi.process <- !is.null(cl) && !debug 
  
  if(is.multi.process) {
    if(wasWorking()) {
      stopCluster(cl)
      cl <- initCluster()
    } else {
      clusterStartWorking()
    }
    clusterExport(
      cl, ".evaluateSingle",
      envir=environment())
    if(is.interactive) updateProgressBar(pb, i, "Cluster OK")
  } else {
    if(is.interactive) updateProgressBar(pb, i, "No Cluster")
  }
  
  sources_id <- V(network)[degree(network, mode="in") == 0]

  while(length(sources_id)) {
    sources <- V(network)[sources_id]$name
    
    if(!is.multi.process) {
      evaluated <- foreach(name=sources, .combine=c) %do% {
        i <- i + 1
        if(is.interactive) updateProgressBar(pb, i, name)
        serie <- .evaluateSingle(name, object)
        list(serie)
      }
    } else {
      evaluated <- foreach(name=sources, .combine=c) %dopar% {
        serie <- .evaluateSingle(name, object)
        if(all(serie - old < 0.00001))
        list(serie)
      }
      i <- i + length(sources)
      if(is.interactive) {
        updateProgressBar(pb, i, last(sources))
      }
    }
   
    names(evaluated) <- sources
    
    if(length(evaluated) == 1) {
      data[[sources]] <- evaluated[[sources]]
    } else {
      data[sources] <- evaluated
    }
    
    network <- delete.vertices(network, sources_id)
    sources_id <- V(network)[degree(network, mode="in") == 0]
  }

  doneWithCluster()
  if(is.interactive) kill(pb)
  object@data <- data
  object
}



testa <- function() {
  for(name in names(g)) {
    if(!is.null(start) && start != name) {
      next
    }
    
    if(!is.null(start) && start == name) {
      start = NULL
    }
    
    ser(g, name)
  }
}

ratio <- function() {
  g=GrafoDB()
  success = 0
  for(name in listAggregates(g)) {
    tryCatch({
      ser(g, name)
      success <- success + 1
    }, error = function(err) {
      cat(paste0(name, ": ", err, "\n"), file="~/tacci.txt", append=T)
      cat(paste0(name,"\n"), file="~/failures.txt", append=T)
    })
  }
  total <- length(listAggregates(g))
  message(success/total * 100, " success rate")
  message(100 - success/total * 100, " failure rate")
}




#' Carica i dati dal DB
#'
#' Carica i dati direttamente dal DB senza necessita' d'inizializzare un `GrafoDB`
#'
#' @name getdb
#' @usage getdb(name, tag)
#' @param name nome serie
#' @param tag id del grafo (default su `cf10`)
#' @return una serie o una lista di serie
#' @export

getdb <- function(name, tag="cf10") {
  settings <- dbSettings()
  username <- whoami()
  password <- flypwd()

  dati <- load_data(
    username,
    password,
    settings$host,
    settings$port,
    settings$dbname, name, tag); 
  
  if(length(dati)==1 && is.bimets(dati[[1]])) {
    dati[[1]]
  } else {
    dati
  }
}


#' Ottiene i dati dal GrafoDB
#'
#' I dati possono provenire direttamente dal Database se non modificati nella sessione
#' corrente; altriumenti vengono restituiti i dati che l'utente ha appena modificato
#' ma non ancora reso persistenti
#'
#' @name .getdata
#' @rdname getdata_internal
#' @usage .getdata(x, i)
#' @include cluster.r RcppExports.R db.r
#' @param x istanza di `GrafoDB`
#' @param i character array di nomi di serie storiche
#' @return ritorna una named list con all'interno le serie storiche. Se l'array e'
#'         di un solo elemento, ritorna direttamente la serie storica
#'         (questo e' un side-effect, non mi piace)
#' @note se i e' un singolo nome e non esiste nel DB, la funzione termina con errore

.getdata <- function(x,i) {
  ## check if changed, then load internal changes
  data <- x@data
  in.data <- intersect(keys(data), i)
  da.caricare.db <- setdiff(i, in.data)
  tag <- x@tag
  from.db <- if(length(da.caricare.db)) {
    if(x@ordinal != 0) {
      tag <- paste0(tag, "p", x@ordinal)
    }
    ret <- getdb(da.caricare.db, tag)
    if(is.bimets(ret)) {
      ret1 <- list()
      ret1[[da.caricare.db]] <- ret
      ret1
    } else {
      ret
    }
  } else {
    list()
  }
  
  ret <- list()
  for(name in names(from.db)) {
    ret[[name]] <- from.db[[name]]
  }
  
  for(name in in.data) {
    ret[[name]] <- data[[name]]
  }
  
  ## controllo di avere tutte le serie
  if(!all(i %in% names(ret))) {
    non.presenti <- setdiff(i, names(ret))
    warning("le seguenti serie non sono presenti: ",
            paste(non.presenti, collapse=", "))
  }
  
  if(length(ret) == 1) {
    ret <- ret[[1]]
  }
  ret
}

.showConflicts <- function(x) {
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  tag <- x@tag
  df <- dbGetPreparedQuery(
    con,
    "select * from conflitti where tag = ?",
    bind.data=tag)

  for(name in df$name) {
    dfserie <- df[df$name == name,]
    originale <- x[[name]]
    sul.db <- from.data.frame(dfserie)
  }
}

.timeStampForTag <- function(tag) {
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  df <- dbGetPreparedQuery(
    con,
    "select last_updated from grafi where tag=?",
    bind.data = tag)
  if(nrow(df)) {
    df$last_updated
  } else {
    0
  }
}

.tagExists <- function(tag) {
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  df <- dbGetPreparedQuery(
    con,
    "select * from grafi where tag=?",
    bind.data = tag)
  nrow(df) > 0
}

.removeConflicts <- function(x, name=NULL) {
  tag <- x@tag
  if(is.character(name)) {
    params <- cbind(tag, name)
    sql <- "delete from conflitti where tag = ? and name =?"
  } else {
    params <- tag
    sql <- "delete from conflitti where tag = ?"
  }
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  dbSendQuery(con, "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE")
  dbBegin(con)
  tryCatch({
    dbGetPreparedQuery(con, sql, bind.data = params)
  }, error = function(err) {
    dbRollback(con)
    stop(err)
  })
  dbCommit(con)
}

.createGraph <- function(x, tag) {
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  dbSendQuery(con, "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE")
  dbBegin(con)
  dbCommit(con)
}

#' Elimina un edizione del grafo
#'
#' Cancella dal database un edizione del grafo partendo dal suo `tag`
#'
#' @name elimina
#' @usage elimina(tag)
#' @param tag `tag` che distingue in modo univoco il grafo ed i suoi dati
#' @export
#' @import DBI RPostgreSQL

elimina <- function(tag) {

  if(is.grafodb(tag)) {
    tag <- tag@tag
  }

  incancellabili <- c("cf10", "biss", "pne", "dbcong", "prim")
  if(tag %in% incancellabili) stop("Non cancellero' mai ", tag)
 
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  dbSendQuery(con, "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE")
  dbBegin(con)
  tryCatch({
    dbGetPreparedQuery(con, "delete from grafi where tag=?", bind.data = tag)
    dbGetPreparedQuery(con, "delete from conflitti where tag=?", bind.data = tag)

    tables <- c("archi", "dati", "metadati", "formule", "history")
    tables <- paste(tables, tag, sep="_")

    for(table in tables) {
      if(dbExistsTable(con, table)) {
        dbGetQuery(con, paste0("drop table if exists ", table))
      }
    }
  }, error = function(err) {
    dbRollback(con)
    stop(err)
  })
  dbCommit(con)
}

.edita <- function(x, name, ...) {
  file <- tempfile(pattern=paste0(name, "-"), fileext=".R")
  new_task <- paste0(name, " = ... # work it")
  if(!isNode(x, name)) {
    deps <- c()
    if(name %in% keys(x@functions)) {
      task <- x@functions[[name]]
    } else {
      task <- new_task
    }
  } else {
    deps <- getDependencies(x, name)
    task <- expr(x, name, echo=F)
    if(is.null(task)) {
      stop("la serie ", name, " e' una serie primitiva")
    }
  }
  old_task <- task
  if(name %in% keys(x@edges)) {
    deps <- x@edges[[name]]
  }  
  
  task <- .clutter_with_params(task, name, deps) 
  write(task, file=file)
  on.exit(file.remove(file))
  utils::file.edit(file, title=name)
  txtsrc <- paste(readLines(file), collapse="\n")
  edited <- .declutter_function(txtsrc)
  
  if(str_trim(edited) == str_trim(old_task)) {
    return(invisible(x))
  }
  
  x@functions[name] <- edited
  params <- list(...)
  tryCatch({
    f <- eval(parse(text=txtsrc))
    dep <- names(as.list(formals(f)))
    if(!is.null(dep)) {
      x@edges[[name]] <- dep
    }
    x[name] = f
  }, error = function(cond) {
    ## la risetto per poterla editare
    x@functions[name] <- edited
    stop(cond)
  })
  invisible(x)
}

.copy <- function(x,y, name) {
  task <- .declutter_function(as.character(getTask(x, name)))
  task <- gsub(paste0("return\\(", name, "\\)$"), "", task)
  y@functions[[name]] = task
  return(invisible(y))
}

.ser <- function(x, name, debug=FALSE) {
  ## that's the dumbest thing in my life, inverting arguments.
  if(!debug) {
    ret <- .evaluateSingle(name, x)
    if(!is.bimets(ret)) {
      stop(name, " non e' un oggetto bimets")
    }
    ret
  } else {
    task <- expr(x, name, echo=FALSE)
    if(is.null(task)) {
      if(name %in% names(x)) {
        stop(name, " non e' una serie con formula")
      } else {
        stop(name, " non e' una serie del grafo")
      }
    }
    funcName <- paste0(name, "_func")
    f <- .clutter_function(task, name, funcName=funcName)
    filetmp <- tempfile(pattern=name, fileext=".R")
    write(f, file=filetmp)

    env <- new.env()
    source(filetmp)
    debug(funcName)
    nomi_padri <- deps(x, name)
    if(is.null(nomi_padri) || length(nomi_padri) == 0) {
      padri <- list()
    } else {
      padri <- x[[nomi_padri]]
    }

    if(length(nomi_padri) == 1) {
      ## boxing
      ppp <- list()
      ppp[[nomi_padri]] <- padri
      padri <- ppp
    }
    
    attach(padri)
    on.exit({
      rm(list=c(funcName), envir=globalenv())
      file.remove(filetmp)
      detach(padri)
    })
    eval(parse(text=paste0(funcName, "()")))    
  }
}


#' Controlla se un nodo e' una foglia
#'
#' Ritorna un array di `logical` uno per ogni elemento in `i`: `TRUE`
#' se l'i-esimo elemento e' una foglia (non ha archi uscenti), altrimenti `FALSE`
#'
#' @name .isLeaf
#' @usage .isLeaf(x, i)
#' @param x istanza di `GrafoDB`
#' @param i array di `character` con i nomi delle serie su cui si vuole
#'          applicare il predicato
#' @return vector di `logical` (stessa lunghezza di `i`) con i risultati
#'         del controllo
#' @rdname isLeaf-internal

.isLeaf <- function(x, i) {
  network <- x@network
  leaves <- V(network)[degree(network, mode="out") == 0]$name  
  all(i %in% leaves)
}

#' Elimina un nodo dal `GrafoDB`
#'
#' L'eliminazione prevede l'eliminazione dai dati, formule, archi e metadati
#'
#' @name .rmNode
#' @usage .rmNode(graph, tsName, recursive)
#' @param graph istanza di `GrafoDB`
#' @param tsName nomi di serie da eliminare
#' @param recursive `TRUE` se l'eliminazione deve essere rivorsiva sugli archi
#'                  uscenti di ogni serie nel parametro `tsName`.
#'                  `FALSE` altrimenti. Se il parametro e' impostato a `FALSE` e'
#'                  condizione necessaria che le serie in `tsName` siano tutte
#'                  foglie, ovvero serie senza archi uscenti
#' @note Metodo interno
#' @seealso rmNode
#' @rdname rmNode-internal

.rmNode <- function(graph, tsName, recursive=FALSE) {
  sono.tutte.foglie = isLeaf(graph, tsName)
  tag <- graph@tag
  if(!recursive && !sono.tutte.foglie) {
    stop(paste("Non posso cancellare serie intermedie senza",
               "farlo ricorsivamente (ATTENZIONE!)"))
  }
  
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  tryCatch({  
    dbSendQuery(con, "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE")
    dbBegin(con)
    
    figlie <- downgrf(graph, tsName)
    
    da.eliminare <- union(figlie, tsName)
    
    ## eliminare i dati
    dbGetPreparedQuery(
      con,
      "delete from dati where tag=? and name=?",
      bind.data = cbind(tag, da.eliminare))
    suppressWarnings(del(da.eliminare, graph@data))
        
    network <- graph@network
    ## eliminare i vertici dal grafo
    network <- network - vertex(da.eliminare)
    ## eliminare le formule
    dbGetPreparedQuery(
      con,
      "delete from formule where tag=? and name=?",
      bind.data = cbind(tag, da.eliminare))
    suppressWarnings(del(da.eliminare, graph@functions))
    ## eliminare gli archi
    dbGetPreparedQuery(
      con,
      "delete from archi where tag=? and (partenza=? or arrivo=?)",
      bind.data = cbind(tag, da.eliminare, da.eliminare))
    
    ## eliminare i metadati
    dbGetPreparedQuery(
      con,
      "delete from metadati where tag=? and name=?",
      bind.data = cbind(tag, da.eliminare))
    
    graph@network <- network
    dbCommit(con)
  }, error = function(cond) {
    dbRollback(con)
    stop(cond)
  })
  graph
}

.getMeta <- function(x, serie, metadato) {
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  df <- dbGetPreparedQuery(
    con,
    "select value from metadati where tag=? and name=? and key=? order by 1",
    bind.data = cbind(x@tag, serie, metadato))
  if(nrow(df)) {
    as.character(df[,1])
  } else {
    character()
  }
}


#' Checks if a TimeSeries is different from another
#'
#' It's a predicate, returns `TRUE` if:
#' \item a - b != 0
#' \item index(a) != index(b)
#'
#' @name tsdiff
#' @usage tsdiff()
#' @param a timeseries
#' @param b timeseries
#' @return `TRUE` if `a`!=`b`, `FALSE` otherwise
#' @export

tsdiff <- function(a, b, thr = .0000001) {
  if(length(a) != length(b)) {
    return(TRUE)
  }

  idiff <- suppressWarnings(index(a) - index(b))
  if(!all(idiff == 0)) {
    return(TRUE)
  }

  any(a-b > thr) 
}
