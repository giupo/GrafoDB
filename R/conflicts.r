
.has_conflicts <- function(x, name=NULL, con=NULL) {
  nrow(getConflicts(x, name = name, con = con)) > 0
}


.removeConflicts <- function(x, name=NULL) {
  tag <- x@tag
  helper <- x@helper
  key <- if (is.character(name)) {
    "REMOVE_CONFLICTS_NAME"
  } else {
    "REMOVE_CONFLICTS"
  }

  con <- build_connection()
  on.exit(disconnect(con))
  tryCatch({
    DBI::dbBegin(con)
    DBI::dbExecute(con, sql_by_key(helper, key, tag = tag, name = name))
    DBI::dbCommit(con)
  }, error = function(err) {
    DBI::dbRollback(con)
    stop(err)
  })
}

#' Ritorna `TRUE` se il grafo ha conflitti
#'
#' @name has_conflicts
#' @param x oggetto R
#' @param name character array di nomi (puo' essere omesso)
#' @param con Connessione al DB
#' @return `TRUE` se l'istanza `x` e' un GrafoDB con conflitti, `FALSE`
#'         altrimenti
#' @examples \dontrun{
#' g <- GrafoDB(...)
#' has_conflicts(g) # dovrebbe essere FALSE
#' ...             # eseguo operazioni come modificare la stessa serie
#' has_conflicts(g) # TRUE
#' has_conflicts(g, SERIE_CON_CONFLITTO) # TRUE
#' has_conflicts(g, SERIE_SENZA_CONFLITTO) # FALSE
#' }
#' @export
#' @include core.r

methods::setGeneric(
  "has_conflicts",
  function(x, name = NULL, con = NULL) {
    standardGeneric("has_conflicts")
  })

#' @rdname has_conflicts

methods::setMethod(
  "has_conflicts",
  signature("GrafoDB", "ANY"),
  function(x, name = NULL, con = NULL) {
    .has_conflicts(x, name = name, con = con)
  })


#' Ritorna i conflitti per un Grafo
#'
#' Ritorna i conflitti di un grafo, tutti per serie specificate dal
#' parametro `name`
#'
#' @name getConflicts
#' @param x istanza del GrafoDB
#' @param name vettore di nomi di serie
#' @param con connection to the DB, if `NULL` the connections 
#'  gets created and closed in the method
#' @return un data.frame con le informazioni del conflitto
#' @export

methods::setGeneric(
  "getConflicts",
  function(x, name = NULL, con = NULL) {
    standardGeneric("getConflicts")
  })

#' @rdname getConflicts

methods::setMethod(
  "getConflicts",
  signature("GrafoDB", "ANY"),
  function(x, name = NULL, con = NULL) {
    con <- if (is.null(con)) {
      con <- build_connection()
      on.exit(disconnect(con))
      con
    } else {
      con
    }

    tag <- x@tag
    helper <- x@helper
    sql <- if (is.null(name)) {
      sql_by_key(helper, "GET_CONFLICTS", tag = tag)
    } else {
      sql_by_key(helper, "GET_CONFLICTS_NAME", tag = tag, name = name)
    }

    DBI::dbGetQuery(con, sql)
  })


methods::setGeneric(
  "getDataConflicts",
  function(x, name = NULL) {
    standardGeneric("getDataConflicts")
  })

methods::setMethod(
  "getDataConflicts",
  signature("GrafoDB", "ANY"),
  function(x, name = NULL) {
    df <- getConflicts(x, name)
    if (nrow(df)) {
      ret <- list()
      lista <- as.character(df$name)
        foreach::`%do%`(foreach::foreach(name = iterators::iter(lista), .combine = rbind), {
        autore <- df[df$name == name, "autore"]
        current_autore <- df[df$name == name, "old_autore"]

        current <- df[
          df$name == name,
          c("name", "old_anno", "old_periodo", "old_freq", "old_dati")
        ]

        colnames(current) <- c("name", "anno", "periodo",
                               "freq", "dati")

        current <- from_data_frame(current)[[name]]

        nuova <- df[
          df$name == name,
          c("name", "anno", "periodo", "freq", "dati")
        ]

        colnames(nuova) <- c("name", "anno", "periodo", "freq", "dati")
        nuova <- from_data_frame(nuova)[[name]]

        differenza <- if (stats::frequency(nuova) == stats::frequency(current)) {
          nuova - current
        } else {
          NA
        }

        ret[[name]] <- cbind(nuova, autore, current, current_autore, differenza)
      })
      ret
    }
  })

methods::setGeneric(
  "getFormulaConflicts",
  function(x, name = NULL) {
    standardGeneric("getFormulaConflicts")
  })

methods::setMethod(
  "getFormulaConflicts",
  signature("GrafoDB", "ANY"),
  function(x, name = NULL) {
    tag <- x@tag
    helper <- x@helper
    con <- build_connection()
    on.exit(disconnect(con))
    sql <- if (is.null(name)) {
      sql_by_key(helper, "GET_FORMULA_CONFLICTS", tag = tag)
    } else {
      sql_by_key(helper, "GET_FORMULA_CONFLICTS_NAME", tag = tag, name = name)
    }
    DBI::dbGetQuery(con, sql)
  })


#' contrassegna come risolti i conflitti salvati sul DB
#'
#' E' appannaggio dell'utente riusolvere i conflitti nella sua sessione
#' e provvedere a salvate un Grafo consistente.
#'
#' @name fixConflicts
#' @param x istanza di GrafoDB
#' @param name nome della serie da cui eliminare un conflitto
#' @export

methods::setGeneric(
  "fixConflicts",
  function(x, name = NULL) {
    standardGeneric("fixConflicts")
  })

#' @rdname fixConflicts

methods::setMethod(
  "fixConflicts",
  signature("GrafoDB"),
  function(x, name = NULL) {
    .removeConflicts(x, name = name)
  })

#' Trova le serie che sono cambiate nel database
#'
#' @name get_changed_series_names
#' @param x istanza di Grafo
#' @param con eventuale connessione al database (se non presente, ne crea una)
#' @return lista di nomi di serie cambiate sul database rispetto ad X
#' @note funzione interna

get_changed_series_names <- function(x, con = NULL) {
  con <- if (is.null(con)) {
    con <- build_connection()
    on.exit(disconnect(con))
    con
  } else {
    con
  }

  tag <- x@tag
  helper <- x@helper
  timestamp <- x@timestamp
  df <- DBI::dbGetQuery(con, sql_by_key(
    helper, "GET_CHANGED_SERIES", tag = tag,
    last_updated = timestamp))
  nomi <- as.character(df$name)
  unique(nomi)
}

get_outer_data_names <- function(x, con = NULL) {
  con <- if (is.null(con)) {
    con <- build_connection()
    on.exit(disconnect(con))
    con
  } else {
    con
  }
  autore <- rutils::whoami()
  tag <- x@tag
  timestamp <- x@timestamp
  helper <- x@helper
  df <- DBI::dbGetQuery(con, sql_by_key(
    helper, "GET_OUTER_DATA_NAMES", tag = tag, last_updated = timestamp,
    autore = autore))
  as.character(df$name)
}

get_outer_formula_names <- function(x, con = NULL) {
  con <- if (is.null(con)) {
    con <- build_connection()
    on.exit(disconnect(con))
    con
  } else {
    con
  }

  autore <- rutils::whoami()
  tag <- x@tag
  timestamp <- x@timestamp
  helper <- x@helper

  df <- DBI::dbGetQuery(con, sql_by_key(
    helper, "GET_OUTER_FORMULA_NAMES",
    tag = tag, last_updated = timestamp,
    autore = autore))

  as.character(df$name)
}

#' @include functions.r
#' @include db.r persistence_utils.r

check_conflicts <- function(x, con = NULL) {
  tag <- x@tag

  data <- x@data
  functions <- x@functions

  names_data <- hash::keys(x@data)
  names_formule <- hash::keys(x@functions)

  outer_names_data <- get_outer_data_names(x, con = con)
  outer_names_formule <- get_outer_formula_names(x, con = con)

  common_data <- intersect(names_data, outer_names_data)
  if (length(common_data) > 0) {
    ## trovo solo le root
    only_roots <- intersect(.roots(x), common_data)
    if (length(only_roots) > 0) {
      ## Controllo una ad una le radici e verifico differenze di valori
      dati_db <- load_data(tag, con = con)
      for (name in unique(only_roots)) {
        outer_data_frame <- dati_db[dati_db$name == name,]
        outer_ts <- convert_data_frame(outer_data_frame)[[name]]
        inner_ts <- x[[name]]
        if (ts_differ(outer_ts, inner_ts)) {
          create_data_conflicts(x, name, con = con)
        }
      }
    }
  }

  common_functions <- intersect(names_formule, outer_names_formule)
  if (length(common_functions) > 0) {
    ## controllo ogni nome per verificare differenze.
    func_in_db <- load_formulas(tag, con = con)
    func_in_db <- func_in_db[func_in_db$name %in% common_functions, ]
    for (name in unique(as.character(func_in_db$name))) {
      formula_db <- func_in_db[func_in_db$name == name, ]$formula
      if (formula_db != functions[[name]]) {
        ## crea conflitto su formule per name
        create_function_conflicts(x, name, formula_db, con = con)
      }
    }
  }

  x
}

#' @include db.r persistence_utils.r

create_data_conflicts <- function(x, nomi, con = NULL) {
  con <- if (is.null(con)) {
    con <- build_connection()
    on.exit(disconnect(con))
    con
  } else {
    con
  }

  tag <- x@tag
  autore <- rutils::whoami()
  helper <- x@helper
  timestamp <- time_in_nano()
  name <- NULL # for checks
  dati <- foreach::`%do%`(foreach::foreach(name = iterators::iter(nomi),
    .combine = rbind), {

    tt <- x[[name]]
    df <-  to_data_frame(tt, name)
    anno <- df$anno
    prd <- df$periodo
    freq <- df$freq
    dati <- df$dati

    tryCatch({
      DBI::dbExecute(con, sql_by_key(
        helper, "CREA_CONFLITTO_DATI",
        tag = tag,
        name = name,
        anno = anno,
        periodo = prd,
        freq = freq,
        dati = dati,
        autore = autore,
        last_updated = timestamp))
    }, error = function(cond) {
      stop(cond)
    })
  })

  warning(
    "Ci sono conflitti sui dati per le serie: ",
    paste(nomi, collapse = ", "))
}

create_function_conflicts <- function(x, nomi, formula_db, con = NULL) {
  con <- if (is.null(con)) {
    con <- build_connection()
    on.exit(disconnect(con))
    con
  } else {
    con
  }

  autore <- rutils::whoami()
  tag <- x@tag
  helper <- x@helper

  timestamp <- time_in_nano()
  name <- NULL # for checks
  foreach::`%do%`(foreach::foreach(name = iterators::iter(nomi)), {
    sql1 <- sql_by_key(
      helper, "CREA_CONFLITTO_FORMULE1",
      formula = formula_db,
      autore = autore,
      name = name,
      tag = tag,
      last_updated = timestamp)

    DBI::dbExecute(con, sql1)

    sql2 <- sql_by_key(
      helper, "CREA_CONFLITTO_FORMULE2",
      formula = formula_db,
      autore = autore,
      name = name,
      tag = tag,
      last_updated = timestamp)
    DBI::dbExecute(con, sql2)
  })

  warning(
    "Ci sono conflitti sulle formule per le serie: ",
    paste(nomi, collapse = ", "))
}
