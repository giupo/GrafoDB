#' Funzione d'inizializzazione del grafo.
#'
#' Questa funzione va utilizzata nell'initialize S4 dell'oggetto `GrafoDB`
#'
#' @name init_grafo_impl
#' @rdname init-internal
#' @param object (creato da new)
#' @param tag tag del grafo (default=`cf10`)
#' @return un istanza di grafo popolata correttamente secono i parametri (`tag`)
#' @note e' stata scorporata dall'initialize S4 per finalita' di debug
#' @include persistence.r sqlhelper.r
#' @include db.r persistence_utils.r

init_grafo_impl <- function(object, tag = "cf10", con = NULL) {
  ln <- "GrafoDB.functions.init_grafo_impl"
  if (is.null(tag)) {
    tag <- "cf10"
  } else {
    tag <- tolower(tag)
  }

  object@edges <- hash::hash()
  object@data <- hash::hash()
  object@functions <- hash::hash()
  object@touched <- character(0)
  object@ordinal <- if (grepl("p(\\d+)$", tag)) {
    mth <- stringr::str_match(tag, "p(\\d+)$")
    as.numeric(mth[1, 2])
  } else {
    0
  }

  debug("GRAFODB_ENV: %s", getenv(), name = ln)
  object@tag <- tag
  object@helper <- SQLHelper()

  if (is.null(con)) {
    con <- build_connection()
    on.exit(disconnect(con))
  }

  archi <- load_edges(tag, con = con)
  object <- resync(object, con = con)

  network <- if (nrow(archi) > 0) {
    archi <- archi[, c("partenza", "arrivo")]
    igraph::graph.data.frame(as.data.frame(archi), directed = TRUE)
  } else {
    igraph::graph.empty(directed = TRUE)
  }

  object@network <- network
  nomi <- names(object)
  if (length(nomi) > 0) {
    pending_names <- setdiff(nomi, igraph::V(network)$name)
    network <- network + igraph::vertex(pending_names)
  }

  object@network <- network

  df <- load_grafi(con)
  dftag <- df[df$tag == tag, ]
  if (nrow(dftag)) {
    ## il grafo esiste nel DB
    object@timestamp <- as.numeric(dftag$last_updated)
    if (interactive()) {
      message(dftag$comment)
    }
  } else {
    ## il grafo non esiste nel DB
    object <- create_new_grafo(object, tag, con = con)
  }

  object
}


#' Controlla se `x` e' un `GrafoDB`
#'
#' Predicato; Ritorna `TRUE` se `x` e' un istanza di `GrafoDB`, altrimenti
#' ritorna `FALSE`
#'
#' @name is.grafodb
#' @param x un qualsiasi oggetto `R`
#' @return `TRUE` se `x` e' un `GrafoDB`, altrimenti `FALSE`
#' @examples \dontrun{
#'   g = GrafoDB()
#'   is.grafodb(g) # questo e' TRUE
#'   x = list()
#'   is.grafodb(x) # questo e' FALSE
#' }
#' @export

is.grafodb <- function(x) { # nolint
  inherits(x, "GrafoDB")
}

#' converte una timeseries `ts`
#' o un generico scalare in un data.frame.
#' funzione utilizzata per convertire il dato in una forma accettabile dal DB
#'
#' @name to_data_frame
#' @param x una timeseries `ts` o uno scalare
#' @param name nome da dare alla timeseries
#' @return una rappresentazione a data.frame della serie `x`
#' @note funzione interna
#' @rdname todataframe

 to_data_frame <- function(x, name=NULL) {
  ## questa funzione converte a dataframe la timeseries,
  ## utile per l'inserimento nel DB
  if (stats::is.ts(x)) {
    anno <- stats::start(x)[[1]]
    prd  <- stats::start(x)[[2]]
    freq <- stats::frequency(x)
  } else {
    anno <- 0
    prd <- 0
    freq <- 0
  }

  # fix per bug su CRCONFAC/PC che assegna names su una serie storica
  names(x) <- NULL
  raw_numbers <- jsonlite::toJSON(x, digits = 20, na = NULL)
  raw_numbers <- as.character(raw_numbers)
  raw_numbers <- gsub(" ", "", raw_numbers)

  if (is.null(name)) {
    as.data.frame(
      list(anno = anno, periodo = prd,
           freq = freq, dati = raw_numbers),
      stringsAsFactors = FALSE)
  } else {
    as.data.frame(
      list(name = name, anno = anno, periodo = prd,
           freq = freq, dati = raw_numbers),
      stringAsFactors = FALSE)
  }
}

#' converte un dataframe (caricato dal Database) in una timeseries `ts`
#'
#' @name from_data_frame
#' @param df data.frame compilato dal database
#' @note i dati dal db sono memorizzati come stringhe JSON
#' @rdname fromdataframe

from_data_frame <- function(df) {
  stopifnot(is.data.frame(df))
  ret <- list()

  for (i in seq(nrow(df))) {
    row <- df[i, ]
    anno <- row$anno
    periodo <- row$periodo
    freq <- row$freq
    params <- c(anno, periodo, freq)
    name <- as.character(if (is.null(row$name)) {
        i
      } else {
        row$name
      })

    if (any(params == 0)) {
      ret[[name]] <- jsonlite::fromJSON(as.character(row$dati))
    } else {
      dati <- stats::ts(
        jsonlite::fromJSON(as.character(row$dati)),
        start = c(anno, periodo),
        frequency = freq)

      ret[[name]] <- dati
    }
  }
  ret
}


#' funzione per eliminare le definizione 'function' dalle formule per il GrafoDB
#'
#' @name declutter_function
#' @param func_string formula in formato testo
#' @return the function without the "function" and curly braces

declutter_function <- function(func_string) {
  func_string <- rutils::ifelse(
    is.function(func_string),
    paste(utils::capture.output(func_string), collapse = "\n"),
    func_string)

  idx_inizio <- stringr::str_locate(func_string, "\\{")[[1]]
  idx_fine <- sapply(gregexpr("\\}", func_string), utils::tail, 1)

  func_string <- substring(func_string, idx_inizio + 1, idx_fine - 1)
  func_string <- gsub("^\n(.*)\n$", "\\1", func_string)
  stringr::str_trim(func_string)
}


#' Orla le formule del grafo in funzioni
#'
#' Questa funzione orla le funzioni del grafo con
#' `proxy <-function() {` e `}` finale.
#'
#' Le istruzioni vengono incapsulate in una funzione generica chiamata proxy.
#' gli argomenti devono essere definiti prima nella ambiente per la
#' corretta esecuzione
#'
#' @name to_function_as_string
#' @param func_string character array che rappresenta la funzione
#' @param name name of the object to be returned
#' @param func_name name of the function (`proxy` default)
#' @return un character array della funzione orlata
#' @note funzione interna

to_function_as_string <- function(func_string, name, func_name = "proxy") {
  glue::glue("{func_name} <- function() {{
    {func_string}
    {name}
  }}")
}


#' questa funzione orla la formula del grafo come una funzione oon parametri
#'
#' I parametri della funzione ritornata sono le dipendenze della serie
#'
#' @name clutter_with_params
#' @param func_string function task to be converted as function
#' @param deps character array di dipendenze
#' @return Ritorna una una funzione `is.character(ret) == TRUE`
#' @rdname clutter_with_params_internal

clutter_with_params <- function(func_string, deps) {
  glue::glue("proxy <- function( {paste(deps, collapse = ', ')} ) {{
  {func_string}
  }}")
}

#' questa funzione orla la formula del grafo come una funzione
#'
#' I parametri della funzione ritornata sono le dipendenze della serie,
#' ed aggiunge il nome della funzione al termine per dichiarare il
#' dato ritornato
#'
#' @name clutter_with_params_and_return
#' @param func_string function task to be converted as function
#' @param name task name
#' @param deps array di dipendenze
#' @param func_name nome della funzione definita
#' @return Ritorna un character array `is.character(ret) == TRUE`
#' @rdname clutter_with_params_and_return_internal

clutter_with_params_and_return <- function(func_string, name,
  deps, func_name = "proxy") {
  glue::glue("{func_name} <- function( {paste(deps, collapse = ', ')} ) {{
    {func_string}
    {name}
  }}")
}



#' Carica i dati dal DB
#'
#' Carica i dati direttamente dal DB senza necessita' d'inizializzare
#' un `GrafoDB`
#'
#' @name getdb
#' @param name nome serie
#' @param tag id del grafo (default su `cf10`)
#' @return una serie o una lista di serie
#' @export

getdb <- function(x, name) {
  dbdati <- x@dbdati
  df <- dbdati[dbdati$name %in% name, ]

  if (nrow(df) == 0) return(list())

  if (length(name) > 1000) {
    foreach::`%dopar%`(foreach::foreach(
      row = iterators::iter(df, by = "row"),
      .combine = c, .multicombine = TRUE), {
      convert_data_frame(row)
    })
  } else {
    convert_data_frame(df)
  }
}


#' Ottiene i dati dal GrafoDB
#'
#' I dati possono provenire direttamente dal Database se non modificati
#' nella sessione corrente; altriumenti vengono restituiti i dati che
#' l'utente ha appena modificato ma non ancora reso persistenti
#'
#' @name get_data
#' @rdname getdata_internal
#' @include db.r
#' @param x istanza di `GrafoDB`
#' @param ids character array di nomi di serie storiche
#' @return ritorna una named list con all'interno le serie storiche.
#'         Se l'array e' di un solo elemento, ritorna direttamente la serie
#'         storica (questo e' un side-effect, non mi piace)
#' @note se ids e' un singolo nome e non esiste nel DB, la funzione termina
#'  con errore

get_data <- function(x, ids) {
  ## check if changed, then load internal changes
  data <- x@data
  in_data <- intersect(hash::keys(data), ids)
  to_be_loaded_from_db <- setdiff(ids, in_data)
  tag <- x@tag
  from_db <- if (length(to_be_loaded_from_db)) {
    if (x@ordinal != 0) {
      tag <- paste0(tag, "p", x@ordinal)
    }
    getdb(x, to_be_loaded_from_db)
  } else {
    list()
  }

  ret <- list()
  for (name in names(from_db)) {
    ret[[name]] <- from_db[[name]]
  }

  for (name in in_data) {
    ret[[name]] <- data[[name]]
  }

  ## controllo di avere tutte le serie
  if (!all(ids %in% names(ret))) {
    not_found <- setdiff(ids, names(ret))
    warning("cannot find the following objects: ",
            paste(not_found, collapse = ", "))
  }

  if (length(ret) == 1) {
    ret <- ret[[1]]
  }
  ret
}

#' @include db.r

exists_tag <- function(tag, con = NULL) {
  con <- if (is.null(con)) {
    con <- build_connection()
    on.exit(disconnect(con))
    con
  } else {
    con
  }

  sql <- glue::glue_sql("select * from grafi where tag = {tag}", .con = con)
  df <- DBI::dbGetQuery(con, sql)
  nrow(df) > 0
}


.copy <- function(x, y, name) {
  task <- declutter_function(as.character(getTask(x, name)))
  task <- gsub(paste0("return\\(", name, "\\)$"), "", task)
  y@functions[[name]] <- task
  return(invisible(y))
}



#' Ritorna le radici del GrafoDB
#'
#' Ritona tutti i nodi del grafo le che non hanno archi entranti
#'
#' @name .roots
#' @param x Grafo
#' @rdname roots-internal
#' @return la lista delle radici del grafo

.roots <- function(x) {
  n <- x@network
  igraph::V(n)[igraph::degree(n, mode = "in") == 0]$name
}

#' Ritorna le figlie del GrafoDB
#'
#' Ritona tutti i nodi del grafo le che non hanno archi uscenti
#'
#' @name .leaves
#' @param x Grafo
#' @rdname leaves-internal
#' @return la lista delle foglie del grafo

.leaves <- function(x) {
  n <- x@network
  igraph::V(n)[igraph::degree(n, mode = "out") == 0]$name
}


#' Controlla se un nodo e' una foglia
#'
#' Ritorna un array di `logical` uno per ogni elemento in `i`: `TRUE`
#' se l'i-esimo elemento e' una foglia (non ha archi uscenti),
#' altrimenti `FALSE`
#'
#' @name .isLeaf
#' @param x istanza di `GrafoDB`
#' @param i array di `character` con i nomi delle serie su cui si vuole
#'          applicare il predicato
#' @return vector di `logical` (stessa lunghezza di `i`) con i risultati
#'         del controllo
#' @rdname isLeaf-internal

.isLeaf <- function(x, i) all(i %in% .leaves(x)) # nolint


#' Controlla se un nodo e' una radice
#'
#' Ritorna un array di `logical` uno per ogni elemento in `i`: `TRUE`
#' se l'i-esimo elemento e' una radice (non ha archi uscenti),
#' altrimenti `FALSE`
#'
#' @name .isRoot
#' @param x istanza di `GrafoDB`
#' @param i array di `character` con i nomi delle serie su cui si vuole
#'          applicare il predicato
#' @return vector di `logical` (stessa lunghezza di `i`) con i risultati
#'         del controllo
#' @rdname isRoot-internal

.isRoot <- function(x, i) all(i %in% .roots(x)) # nolint


#' Checks if a TimeSeries is different from another
#'
#' It's a predicate, returns `TRUE` if:
#' \itemize{
#' \item a - b != 0
#' \item index(a) != index(b)
#' }
#' @name tsdiff
#' @param a timeseries
#' @param b timeseries
#' @param thr threshold for difference
#' @return `TRUE` if `a`!=`b`, `FALSE` otherwise
#' @export

tsdiff <- function(a, b, thr = .0000001) {
  if (length(a) != length(b)) {
    return(TRUE)
  }

  idiff <- suppressWarnings(zoo::index(a) - zoo::index(b))
  if (!all(idiff == 0)) {
    return(TRUE)
  }

  any(a - b > thr)
}



#' Ritorna la lista dei rilasci presenti nel database
#'
#' @name rilasci
#' @param filtro filtro da applicare alla ricerca sul tag del grafo
#' @export
#' @examples \dontrun{
#'    rilasci() # ritorna tutti i rilasci
#'    rilasci("cf") # ritorna tutti i rilasci con contenenti cf nel tag
#' }
#' @return data.frame con tutti i rilasci

rilasci <- function(filtro = NULL) {
  con <- build_connection()
  on.exit(disconnect(con))
  helper <- SQLHelper()
  sql <- if (is.null(filtro)) {
    sql_by_key(helper, "TUTTI_RILASCI")
  } else {
    sql_by_key(helper, "TUTTI_RILASCI_FILTERED", filtro = filtro)
  }

  data <- DBI::dbGetQuery(con, sql)

  nomicol <- colnames(data)
  if (nrow(data) > 1) {
    time_col <- as.POSIXct(
      as.numeric(data$last_updated) / 1000,
      origin = as.Date("1970-01-01"))

    data <- cbind(data, time_col)
    nomicol <- c(nomicol, "date")
    colnames(data) <- nomicol
  }
  data
}


#' Alias constructor for GrafoDB
#' 
#' @name grafodb <- 
#' @param ... params passed to GrafoDB
#' @return instance of GrafoDB
#' @seealso GrafoDB::GrafoDB
#' @export

grafodb <- function(...) {
  GrafoDB(...)
}

#' Alias constructor for GrafoDB
#' 
#' @name grafo
#' @param ... params passed to GrafoDB
#' @return instance of GrafoDB
#' @seealso GrafoDB::GrafoDB
#' @export

grafo <- function(...) {
  GrafoDB(...)
}