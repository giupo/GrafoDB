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
    edges$id <- NULL ## cancello gli id
    edges$tag <- NULL ## cancello il tag
    edges$last_updated <- NULL
    edges$autore <- NULL
    graph.data.frame(edges,directed=TRUE)
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
    for(name in pending.names) {
      network <- network + vertex(name)
    }
  }
  
  df <- dbGetPreparedQuery(
    con,
    "select * from grafi where tag = ?",
    bind.data = tag)
  
  if(nrow(df)) {
    .Object@timestamp <- df$last_updated
    if(interactive()) {
      message(df$comment)
    }
  } else {
    .Object@timestamp <- Sys.time()
  }
  
  .Object@network <- network
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
#' @examples
#'   g = GrafoDB()
#'   is.grafodb(g) # questo e' TRUE
#'   x = list()
#'   is.grafodb(x) # questo e' FALSE
#'
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
  dati <- TSERIES(
    fromJSON(as.character(df$dati), nullValue = NA),
    START=c(df$anno, df$periodo),
    FREQ=df$freq)
  ret <- list(dati)
  names(ret) <- df$name
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
#' @return un character array della funzione orlata
#' @note funzione interna
#' @rdname clutter_function

.clutter_function <- function(f, name) {
  template <- "proxy <- function() {
--FUNCTION--
--NAME--
  }"

  task <- gsub("--FUNCTION--", f, template)
  task <- gsub("--NAME--", name, task)
  task
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
  tsformula <- expr(graph, name, echo=FALSE)
  nomi_padri <- upgrf(graph, name, livello=1)
  if(length(nomi_padri) == 0) {
    return(graph[[name]])
  }
  padri <- graph[[nomi_padri]]
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
    traceback()
    stop(name, ": ", err)
  }, warning = function(warn) {
    traceback()
    stop(name, ": ", warn)
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

.evaluate <- function(object, v_start=NULL, deep=T, ...) {
  data <- object@data
  network <- object@network
  all_names <- names(object)
  if(!all(v_start %in% all_names)) {
    not.in.graph <- setdiff(v_start, all_names)
    stop("Non sono serie del grafo: ", paste(not.in.graph, collapse=", "))
  }

  if(is.null(v_start)) {
    ## alla prima tornata elimino le serie primitive perche' prive di formule.
    sources_id<- V(network)[degree(network, mode="in") == 0]
    network <- delete.vertices(network, sources_id)
  } else {
    v_start <- as.character(v_start)
    network <- induced.subgraph(
      network,
      V(network)[unlist(
        neighborhood(network, order=.Machine$integer.max, nodes=v_start, mode="out")
      )])
  }

  #se il network e' vuoto dopo l'eliminazione delle sorgenti, ritorno senza fare nulla
  if(!length(V(network))) {
    return(invisible(object))
  }

  deep <- as.logical(deep)

  cl <- initDefaultCluster()

  total <- length(V(network))
  i = 0
  pb <- txtProgressBar(min=0, max=total)
  ## trovo le fonti
  sources_id <- V(network)[degree(network, mode="in") == 0]
  while(length(sources_id)) {
    sources <- V(network)[sources_id]$name

    evaluated.data <- clusterParLapply(sources, function(name, object) {
      .evaluateSingle(name, object)
    }, object)

    names(evaluated.data) <- sources
    for(name in names(evaluated.data)) {
      data[[name]] <- evaluated.data[[name]]
    }
    i <- i + length(evaluated.data)
    setTxtProgressBar(pb, i)
    network <- delete.vertices(network, sources_id)
    sources_id <- V(network)[degree(network, mode="in") == 0]
  }
  close(pb)
  object@data <- data
  object
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
#' @param x istanza di `GrafoDB`
#' @param i character array di nomi di serie storiche
#' @return ritorna una named list con all'interno le serie storiche. Se l'array e' di un solo elemento, ritorna direttamente la serie storica (questo e' un side-effect, non mi piace)
#' @note se i e' un singolo nome e non esiste nel DB, la funzione termina con errore

.getdata <- function(x,i) {
  ## check if changed, then load internal changes
  data <- x@data
  in.data <- intersect(keys(data), i)
  da.caricare.db <- setdiff(i, in.data)
  from.db <- if(length(da.caricare.db)) {
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    params <- as.data.frame(cbind(x@tag, da.caricare.db))
    names(params) <- c("tag", "name")
    sub.df <- dbGetPreparedQuery(
      con,
      "select name, anno, periodo, freq, dati from dati where tag = ? and name = ? ",
      bind.data = cbind(x@tag, da.caricare.db))
    CLUSTER_LIMIT <- getOption("CLUSTER_LIMIT", 100)
    if(length(i) == 1) {
      tempret <- list()
      tempret[[i]] <- tryCatch({
        from.data.frame(sub.df[1,])[[i]]
      }, error=function(err) {
        stop("La serie ",i," non esiste nel Grafo")
      })
      tempret
    } else if (length(i) > CLUSTER_LIMIT) {
      .initDefaultCluster()
      foreach(row=iter(sub.df, by='row'), .combine=append) %dopar% from.data.frame(row)
    } else {
      foreach(row=iter(sub.df, by='row'), .combine=append) %do% from.data.frame(row)
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
    warning("le seguenti serie non sono presenti: ", paste(non.presenti, collapse=", "))
  }

  if(length(ret) == 1) {
    ret <- ret[[1]]
  }
  ret
}

#' edita una formula del `GrafoDB`
#'
#' E' un metodo di tipo S3 per essere compliant con la definizione di `edit`
#'
#' @name edit.GrafoDB
#' @usage edit(g, nome)
#' @usage edit.GrafoDB(g, nome)
#' @param g istanza di grafo
#' @param name nome della serie da editare
#' @export

edit.GrafoDB <- function(g, name) {
  task <- getTask(g, name)
  task <- .clutter_function(task, name)
  task <- eval(parse(text=task))
  task <- edit(task)
  task <- .declutter_function(task)
  g@functions[[name]] <- task
  invisible(g)
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
