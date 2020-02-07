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
#' @include persistence.r sqlhelper.r
#' @include db.r persistence_utils.r

.init <- function(.Object, tag="cf10", con = NULL) {
  ln <- "GrafoDB.functions.init"
  if(is.null(tag)) {
    tag <- "cf10"
  } else {
    tag <- tolower(tag)
  }

  .Object@edges <- hash::hash()
  .Object@data <- hash::hash()
  .Object@functions <- hash::hash()
  .Object@touched <- character(0)
  .Object@ordinal <- if(grepl("p(\\d+)$", tag)) {
    mth <- stringr::str_match(tag, "p(\\d+)$")
    as.numeric(mth[1,2])
  } else {
    0
  }

  flog.debug("GRAFODB_ENV: %s", getenv(), name=ln)
  .Object@tag <- tag
  .Object@helper <- SQLHelper()

  if (is.null(con)) {
    con <- buildConnection()
    on.exit(disconnect(con))
  }

  archi <- loadArchi(tag, con=con)
  .Object <- resync(.Object, con=con)

  network <- if(nrow(archi) > 0) {
    archi <- archi[, c("partenza", "arrivo")]
    igraph::graph.data.frame(as.data.frame(archi), directed=TRUE)
  } else {
    igraph::graph.empty(directed=TRUE)
  }

  .Object@network <- network
  nomi <- names(.Object)
  if(length(nomi) >0 ) {
    pending.names <- setdiff(nomi, V(network)$name)
    network <- network + igraph::vertex(pending.names)
  }

  .Object@network <- network

  df <- loadGrafi(con)
  dftag <- df[df$tag == tag,]
  if(nrow(dftag)) {
    ## il grafo esiste nel DB
    .Object@timestamp <- as.numeric(dftag$last_updated)
    if(interactive()) {
      message(dftag$comment)
    }
  } else {
    ## il grafo non esiste nel DB
    .Object <- createNewGrafo(.Object, tag, con=con)
  }

  .Object
}


#' Controlla se `x` e' un `GrafoDB`
#'
#' Predicato; Ritorna `TRUE` se `x` e' un istanza di `GrafoDB`, altrimenti
#' ritorna `FALSE`
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

#' converte una timeseries `ts`
#' o un generico scalare in un data.frame.
#' funzione utilizzata per convertire il dato in una forma accettabile dal DB
#'
#' @name to.data.frame
#' @usage to.data.frame(x)
#' @param x una timeseries `ts` o uno scalare
#' @param name nome da dare alla timeseries
#' @return una rappresentazione a data.frame della serie `x`
#' @note funzione interna
#' @rdname todataframe

to.data.frame <- function(x, name=NULL) {
  ## questa funzione converte a dataframe la timeseries,
  ## utile per l'inserimento nel DB
  if(is.ts(x)) {
    anno <- stats::start(x)[[1]]
    prd  <- stats::start(x)[[2]]
    freq <- stats::frequency(x)
  } else {
    anno <- 0
    prd <- 0
    freq <- 0
  }

  # fix per bug su CRCONFAC/PC che assegna names su una serie storica
  names(x) = NULL
  raw_numbers <- jsonlite::toJSON(x, digits=20, na=NULL)
  raw_numbers <- as.character(raw_numbers)
  raw_numbers <- gsub(" ", "", raw_numbers)

  if(is.null(name)) {
    as.data.frame(
      list(anno=anno, periodo=prd,
           freq=freq, dati=raw_numbers),
      stringsAsFactors = FALSE)
  } else {
    as.data.frame(
      list(name=name, anno=anno, periodo=prd,
           freq=freq, dati=raw_numbers),
      stringAsFactors = FALSE)
  }
}

#' converte un dataframe (caricato dal Database) in una timeseries `ts`
#'
#' @name from.data.frame
#' @usage from.data.frame(df)
#' @param df data.frame compilato dal database
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
    name <- as.character(if(is.null(row$name)) {
      i
    } else {
      row$name
    })
    if(any(params == 0)) {
      ret[[name]] <- jsonlite::fromJSON(as.character(row$dati))
    } else {
      dati <- ts(
        jsonlite::fromJSON(as.character(row$dati)),
        start=c(anno, periodo),
        frequency=freq)
      ret[[name]] <- dati
    }
  }
  ret
}


#' funzione per eliminare le definizione 'function' dalle formule per il GrafoDB
#'
#' @name .declutter_function
#' @usage .declutter_function(f)
#' @param f formula in formato testo
#' @rdname declutter-function-internal

.declutter_function <- function(f) {
  f <- if(is.function(f)) {
    # f <- paste(deparse(f), collapse="\n")
    f <- capture.output(f)
    #f <- f[1:length(f)-1]
    f <- paste(f, collapse="\n")
  } else {
    f
  }

  idx_inizio <- stringr::str_locate(f, "\\{")[[1]]
  idx_fine <- sapply(gregexpr("\\}", f), tail, 1)

  f <- substring(f, idx_inizio + 1, idx_fine - 1)
  f <- gsub("^\n(.*)\n$", "\\1", f)
  stringr::str_trim(f)
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


#' Carica i dati dal DB
#'
#' Carica i dati direttamente dal DB senza necessita' d'inizializzare un `GrafoDB`
#'
#' @name getdb
#' @usage getdb(name, tag)
#' @param name nome serie
#' @param tag id del grafo (default su `cf10`)
#' @return una serie o una lista di serie
#' @importFrom foreach %do% %dopar%
#' @export

getdb <- function(x, name) {
  dbdati <- x@dbdati
  df <- dbdati[dbdati$name %in% name, ]

  if (nrow(df) == 0) {
    return(list())
  }


  if(length(name) > 1000) {
    foreach::foreach(row=iterators::iter(df, by='row'), .combine=c, .multicombine=TRUE) %dopar% {
      convert_data_frame(row)
    }
  } else {
    convert_data_frame(df)
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
#' @include db.r
#' @param x istanza di `GrafoDB`
#' @param i character array di nomi di serie storiche
#' @return ritorna una named list con all'interno le serie storiche. Se l'array e'
#'         di un solo elemento, ritorna direttamente la serie storica
#'         (questo e' un side-effect, non mi piace)
#' @note se i e' un singolo nome e non esiste nel DB, la funzione termina con errore

.getdata <- function(x,i) {
  ## check if changed, then load internal changes
  data <- x@data
  in.data <- intersect(hash::keys(data), i)
  da.caricare.db <- setdiff(i, in.data)
  tag <- x@tag
  from.db <- if(length(da.caricare.db)) {
    if(x@ordinal != 0) {
      tag <- paste0(tag, "p", x@ordinal)
    }
    da.db <- getdb(x, da.caricare.db)
    #if(length(names(ret)) != length(da.caricare.db)) {
    #  stop("You asked for ", paste(da.caricare.db, collapse=", "),
    #       "but I only got ", paste(names(ret), collapse=", "),
    #       " from DB: check your data now!")
    #}
    da.db
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

#' @include db.r

.tagExists <- function(tag, con=NULL) {
  con <- if(is.null(con)) {
    con <- buildConnection()
    on.exit(disconnect(con))
    con
  } else {
    con
  }

  df <- DBI::dbGetQuery(con, paste0("select * from grafi where tag='", tag,"'"))
  nrow(df) > 0
}


.copy <- function(x,y, name) {
  task <- .declutter_function(as.character(getTask(x, name)))
  task <- gsub(paste0("return\\(", name, "\\)$"), "", task)
  y@functions[[name]] = task
  return(invisible(y))
}



#' Ritorna le radici del GrafoDB
#'
#' Ritona tutti i nodi del grafo le che non hanno archi entranti
#'
#' @name .roots
#' @usage .roots(x)
#' @param x Grafo
#' @rdname roots-internal
#' @return la lista delle radici del grafo

.roots <- function(x) {
  n <- x@network
  igraph::V(n)[igraph::degree(n, mode="in") == 0]$name
}

#' Ritorna le figlie del GrafoDB
#'
#' Ritona tutti i nodi del grafo le che non hanno archi uscenti
#'
#' @name .leaves
#' @usage .leaves(x)
#' @param x Grafo
#' @rdname leaves-internal
#' @return la lista delle foglie del grafo

.leaves <- function(x) {
  n <- x@network
  igraph::V(n)[igraph::degree(n, mode="out") == 0]$name
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

.isLeaf <- function(x, i) all(i %in% .leaves(x))


#' Controlla se un nodo e' una radice
#'
#' Ritorna un array di `logical` uno per ogni elemento in `i`: `TRUE`
#' se l'i-esimo elemento e' una radice (non ha archi uscenti), altrimenti `FALSE`
#'
#' @name .isRoot
#' @usage .isRoot(x, i)
#' @param x istanza di `GrafoDB`
#' @param i array di `character` con i nomi delle serie su cui si vuole
#'          applicare il predicato
#' @return vector di `logical` (stessa lunghezza di `i`) con i risultati
#'         del controllo
#' @rdname isRoot-internal

.isRoot <- function(x, i) all(i %in% .roots(x))


#' Checks if a TimeSeries is different from another
#'
#' It's a predicate, returns `TRUE` if:
#' \itemize{
#' \item a - b != 0
#' \item index(a) != index(b)
#' }
#' @name tsdiff
#' @usage tsdiff()
#' @param a timeseries
#' @param b timeseries
#' @importFrom zoo index
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



#' Ritorna la lista dei rilasci presenti nel database
#'
#' @name rilasci
#' @usage rilasci()
#' @param filtro filtro da applicare alla ricerca sul tag del grafo
#' @export
#' @examples \dontrun{
#'    rilasci() # ritorna tutti i rilasci
#'    rilasci("cf") # ritorna tutti i rilasci con contenenti cf nel tag
#' }
#' @return data.frame con tutti i rilasci

rilasci <- function(filtro=NULL) {
  con <- buildConnection()
  on.exit(disconnect(con))
  helper <- SQLHelper()
  sql <- if(is.null(filtro)) {
    getSQLbyKey(helper, "TUTTI_RILASCI")
  } else {
    getSQLbyKey(helper, "TUTTI_RILASCI_FILTERED", filtro=filtro)
  }

  data <- DBI::dbGetQuery(con, sql)

  nomicol <- colnames(data)
  if(nrow(data) > 1) {
    time_col <- as.POSIXct(
      as.numeric(data$last_updated)/1000,
      origin=as.Date("1970-01-01"))

    data <- cbind(data, time_col)
    nomicol <- c(nomicol, "date")
    colnames(data) <- nomicol
  }
  data
}
