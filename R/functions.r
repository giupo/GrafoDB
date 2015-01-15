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
#' @include persistence.r

.init <- function(.Object, tag="cf10") {
  if(is.null(tag)) {
    tag <- "cf10"
  }
  .Object@tag <- tag
  .Object@data <- hash()
  .Object@functions <- hash()
  
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  archi_table_name <- paste0("archi_", tag)
  
  network <- if(dbExistsTable(con, archi_table_name)) {
    edges <- dbReadTable(con, paste0("archi_", tag))
    if(nrow(edges)) {
      edges$id <- NULL ## cancello gli id
      edges$tag <- NULL ## cancello il tag
      edges$last_updated <- NULL
      edges$autore <- NULL
      graph.data.frame(edges,directed=TRUE)
    } else {
      graph.empty(directed=TRUE)
    }
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

#' converte una timeseries `BIMETS` in un data.frame.
#' funzione utilizzata per convertire il dato in una forma accettabile dal DB
#'
#' @name to.data.frame
#' @usage to.data.frame(x)
#' @param x una timeseries `BIMETS`
#' @param name nome da dare alla timeseries
#' @return una rappresentazione a data.frame della serie `x`
#' @note funzione interna
#' @rdname todataframe

to.data.frame <- function(x, name=NULL) {
  ## questa funzione converte a dataframe la timeseries,
  ## utile per l'inserimento nel DB
  anno <- TSINFO(x, MODE="STARTY")
  prd  <- TSINFO(x, MODE="STARTP")
  freq <- TSINFO(x, MODE="FREQ")
  raw_numbers <- gsub(" ", "", toJSON(x, digits=20))

  if(is.null(name)) {
    as.data.frame(
      list(anno=anno, periodo=prd, freq=freq, dati=raw_numbers), stringsAsFactors = F)
  } else {
    as.data.frame(
      list(name=name, anno=anno, periodo=prd, freq=freq, dati=raw_numbers), stringAsFactors = F)
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
    dati <- TSERIES(
      fromJSON(as.character(row$dati), nullValue = NA),
      START=c(row$anno, row$periodo),
      FREQ=row$freq)
    ret[[row$name]] <- dati
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
#' @param character array di dipendenze
#' @return Ritorna una una funzione `is.function(ret) == TRUE`
#' @rdname clutter_with_params_internal

.clutter_with_params <- function(f, name, deps) {
  template <- "proxy <- function(--DEPS--) {
--FUNCTION--
  }"
  task <- gsub("--DEPS--", paste(deps, collapse = ", "), template)
  task <- gsub("--FUNCTION--", f, task)
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
  
  
  in.functions <- foreach(row=iter(in.functions, by='row'), .combine=rbind) %do% {
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

.evaluateSingle <- function(name, graph) {
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
    ##getData(graph, name)
  })           
}

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
#' @include progressbar.r

.evaluate <- function(object, v_start=NULL, deep=T, ...) {
  params <- list(...)
  debug <- if("debug" %in% names(params)) {
    as.logical(params[["debug"]])
  } else {
    FALSE
  }

  
  
  data <- object@data
  network <- object@network
  all_names <- names(object)
  if(!all(v_start %in% all_names)) {
    not.in.graph <- setdiff(v_start, all_names)
    stop("Non sono serie del grafo: ", paste(not.in.graph, collapse=", "))
  }
  
  if(is.null(v_start)) {
    sources_id <- V(network)[degree(network, mode="in") == 0]
    network <- delete.vertices(network, sources_id)
  } else {
    v_start <- as.character(v_start)
    network <- induced.subgraph(
      network,
      V(network)[unlist(
        neighborhood(network, order=.Machine$integer.max, nodes=v_start, mode="out")
      )])
  }
  
  ## se il network e' vuoto dopo l'eliminazione delle sorgenti, ritorno senza fare nulla
  if(!length(V(network))) {
    return(invisible(object))
  }
  
  deep <- as.logical(deep)
  
  total <- length(V(network))
  i <- 0
  pb <- ProgressBar(min=0, max=total)
  ## trovo le fonti
  sources_id <- V(network)[degree(network, mode="in") == 0]
  cl <- initCluster()
  is.multi.process <- !is.null(cl) && !debug 
  
  while(length(sources_id)) {
    sources <- V(network)[sources_id]$name
    
    evaluated.data <- if(!is.multi.process) {
      lapply(sources, function(name, object) {
        i <- i + 1
        update(pb, i, name)
        .evaluateSingle(name, object)
      }, object)
    } else {
      evaluated.data <- foreach(name = sources, .combine = c) %dopar% {
        serie <- .evaluateSingle(name, object)
        list(serie)
      }
      i <- i + length(evaluated.data)
      update(pb, i, last(sources))
      evaluated.data
    }
    
    names(evaluated.data) <- sources
    
    if(length(evaluated.data) == 1) {
      data[[sources]] <- evaluated.data[[sources]]
    } else {
      data[sources] <- evaluated.data
    }
    
    network <- delete.vertices(network, sources_id)
    sources_id <- V(network)[degree(network, mode="in") == 0]
  }
  kill(pb)
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

.getdata_few <- function(x, i) {
  ## check if changed, then load internal changes
  data <- x@data
  in.data <- intersect(keys(data), i)
  da.caricare.db <- setdiff(i, in.data)
  tag <- x@tag
  from.db <- if(length(da.caricare.db)) {
    ## assurdo ma conviene caricare tutta la tabella e poi discernere
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    params <- cbind(tag, i)
    names(params) <- c("tag", "name")
    df <- dbGetPreparedQuery(
      con,
      paste0("select name, anno, periodo, freq, dati ",
             " from dati where tag = ? and name = ?"),
      bind.data = params)
    
    closure <- function(name, df) {
      sub.df <- df[df$name == name,]
      from.data.frame(sub.df)
    }
    cl <- initCluster()
    if(is.null(cl)) {
      foreach(row=iter(da.caricare.db, by='row'), .combine=append) %do% {
        closure(row, df)
      }
    } else {
      foreach(row=iter(da.caricare.db, by='row'), .combine=append) %dopar% {
        closure(row, df)
      }
    }
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

.getdata_lots <- function(x, i) {
  ## check if changed, then load internal changes
  data <- x@data
  in.data <- intersect(keys(data), i)
  da.caricare.db <- setdiff(i, in.data)
  tag <- x@tag
  from.db <- if(length(da.caricare.db)) {
    ## assurdo ma conviene caricare tutta la tabella e poi discernere
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    
    df <- dbReadTable(con, paste0("dati_", tag))
    
    closure <- function(name, df) {
      sub.df <- df[df$name == name,]
      from.data.frame(sub.df)
    }
    
    cl <- initCluster()
    if(is.null(cl)) {
      foreach(row=iter(da.caricare.db, by='row'), .combine=append) %do% {
        closure(row,df)
      }
    } else {
      foreach(row=iter(da.caricare.db, by='row'), .combine=append) %dopar% {
        closure(row, df)
      }
    } 
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
#' Ottiene i dati dal GrafoDB
#'
#' I dati possono provenire direttamente dal Database se non modificati nella sessione
#' corrente; altriumenti vengono restituiti i dati che l'utente ha appena modificato
#' ma non ancora reso persistenti
#'
#' @name .getdata
#' @rdname getdata_internal
#' @usage .getdata(x, i)
#' @include cluster.r
#' @param x istanza di `GrafoDB`
#' @param i character array di nomi di serie storiche
#' @return ritorna una named list con all'interno le serie storiche. Se l'array e'
#'         di un solo elemento, ritorna direttamente la serie storica
#'         (questo e' un side-effect, non mi piace)
#' @note se i e' un singolo nome e non esiste nel DB, la funzione termina con errore

.getdata <- function(x,i) {
  cl <- initCluster()
  if(length(i) <= 30) {
    .getdata_few(x, i)
  } else {
    .getdata_lots(x, i)
  }
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

  if(tag == "cf10") stop("Non cancellero' mai cf10")
  
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  dbSendQuery(con, "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE")
  dbBegin(con)
  tryCatch({
    dbGetPreparedQuery(con, "delete from grafi where tag=?", bind.data = tag)
    dbGetPreparedQuery(con, "delete from conflitti where tag=?", bind.data = tag)
    dbGetQuery(con, paste0("drop table if exists archi_", tag))
    dbGetQuery(con, paste0("drop table if exists dati_", tag))
    dbGetQuery(con, paste0("drop table if exists metadati_", tag))
    dbGetQuery(con, paste0("drop table if exists formule_", tag))
  }, error = function(err) {
    dbRollback(con)
    stop(err)
  })
  dbCommit(con)
}

.edita <- function(x, name, ...) {
  file <- tempfile(pattern=paste0(name, "-"), fileext=".R")
  deps <- getDependencies(x, name)
  task <- expr(x, name, echo=F)
  write(.clutter_with_params(task, name, deps), file=file)
  on.exit(file.remove(file))
  file.edit(file)
  txtsrc <- paste(readLines(file), collapse="\n")
  x@functions[name] <- .declutter_function(txtsrc)
  f <- eval(parse(text=txtsrc))
  params <- list(...)
  if("eval" %in% names(params) && as.logical(params[["eval"]])) {
    x[name] = f
  }
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
    .evaluateSingle(name, x)
  } else {
    task <- expr(x, name, echo=FALSE)
    f <- .clutter_function(task, name, funcName=name)
    filetmp <- tempfile(pattern=name, fileext=".R")
    write(f, file=filetmp)
    
    source(filetmp)
    debug(name)
    padri <- deps(x, name)
    padri <- x[[padri]]
    attach(padri)
    on.exit({
      file.remove(filetmp)
      detach()
    })
    eval(parse(text=paste0(name, "()")))
  }
}
