
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

  con <- buildConnection()
  on.exit(disconnect(con))
  tryCatch({
    DBI::dbBegin(con)
    DBI::dbExecute(con, getSQLbyKey(helper, key, tag = tag, name = name))
    DBI::dbCommit(con)
  }, error = function(err) {
    DBI::dbRollback(con)
    stop(err)
  })
}

#' Ritorna `TRUE` se il grafo ha conflitti
#'
#' @name has_conflicts
#' @usage has_conflicts(x)
#' @usage has_conflicts(x, name)
#' @param x oggetto R
#' @param name character array di nomi (puo' essere omesso)
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

setGeneric(
  "has_conflicts",
  function(x, name = NULL, con = NULL) {
    standardGeneric("has_conflicts")
  })

#' Ritorna `TRUE` se il grafo ha conflitti
#'
#' @name has_conflicts
#' @usage has_conflicts(x)
#' @usage has_conflicts(x, name)
#' @param x oggetto R
#' @param name character array di nomi (puo' essere omesso)
#' @return `TRUE` se l'istanza `x` e' un GrafoDB con conflitti,
#'         `FALSE` altrimenti
#' @examples \dontrun{
#' g <- GrafoDB(...)
#' has_conflicts(g) # dovrebbe essere FALSE
#' ...             # eseguo operazioni come modificare la stessa serie
#' has_conflicts(g) # TRUE
#' has_conflicts(g, SERIE_CON_CONFLITTO) # TRUE
#' has_conflicts(g, SERIE_SENZA_CONFLITTO) # FALSE
#' }
#' @export
#' @include db.r

setMethod(
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
#' @usage getConflicts(x)
#' @usage getConflicts(x, name)
#' @param x istanza del GrafoDB
#' @param name vettore di nomi di serie
#' @return un data.frame con le informazioni del conflitto
#' @export

setGeneric(
  "getConflicts",
  function(x, name = NULL, con = NULL) {
    standardGeneric("getConflicts")
  })

setMethod(
  "getConflicts",
  signature("GrafoDB", "ANY"),
  function(x, name = NULL, con = NULL) {
    con <- if (is.null(con)) {
      con <- buildConnection()
      on.exit(disconnect(con))
      con
    } else {
      con
    }

    tag <- x@tag
    helper <- x@helper
    sql <- if (is.null(name)) {
      getSQLbyKey(helper, "GET_CONFLICTS", tag = tag)
    } else {
      getSQLbyKey(helper, "GET_CONFLICTS_NAME", tag = tag, name = name)
    }

    DBI::dbGetQuery(con, sql)
  })


setGeneric(
  "getDataConflicts",
  function(x, name = NULL) {
    standardGeneric("getDataConflicts")
  })

setMethod(
  "getDataConflicts",
  signature("GrafoDB", "ANY"),
  function(x, name = NULL) {
    df <- getConflicts(x, name)
    if (nrow(df)) {
      ret <- list()
      lista <- as.character(df$name)
        foreach::foreach(name = iterators::iter(lista), .combine = rbind) %do% {
        autore <- df[df$name == name, "autore"]
        current_autore <- df[df$name == name, "old_autore"]

        current <- df[
          df$name == name,
          c("name", "old_anno", "old_periodo", "old_freq", "old_dati")
        ]

        colnames(current) <- c("name", "anno", "periodo",
                               "freq", "dati")

        current <- from.data.frame(current)[[name]]

        nuova <- df[
          df$name == name,
          c("name", "anno", "periodo", "freq", "dati")
        ]

        colnames(nuova) <- c("name", "anno", "periodo", "freq", "dati")
        nuova <- from.data.frame(nuova)[[name]]

        differenza <- if (stats::frequency(nuova) == stats::frequency(current)) {
          nuova - current
        } else {
          NA
        }

        ret[[name]] <- cbind(nuova, autore, current, current_autore, differenza)
      }
      ret
    }
  })

setGeneric(
  "getFormulaConflicts",
  function(x, name = NULL) {
    standardGeneric("getFormulaConflicts")
  })

setMethod(
  "getFormulaConflicts",
  signature("GrafoDB", "ANY"),
  function(x, name = NULL) {
    tag <- x@tag
    helper <- x@helper
    con <- buildConnection()
    on.exit(disconnect(con))
    sql <- if (is.null(name)) {
      getSQLbyKey(helper, "GET_FORMULA_CONFLICTS", tag = tag)
    } else {
      getSQLbyKey(helper, "GET_FORMULA_CONFLICTS_NAME", tag = tag, name = name)
    }
    DBI::dbGetQuery(con, sql)
  })


#' contrassegna come risolti i conflitti salvati sul DB
#'
#' E' appannaggio dell'utente riusolvere i conflitti nella sua sessione
#' e provvedere a salvate un Grafo consistente.
#'
#' @name fixConflicts
#' @usage fixConflicts(x)
#' @usage fixConflicts(x, name)
#' @param x istanza di GrafoDB
#' @param name nome della serie da cui eliminare un conflitto
#' @export

setGeneric(
  "fixConflicts",
  function(x, name = NULL) {
    standardGeneric("fixConflicts")
  })

setMethod(
  "fixConflicts",
  signature("GrafoDB"),
  function(x, name = NULL) {
    .removeConflicts(x, name = name)
  })

#' Trova le serie che sono cambiate nel database
#'
#' @name get_changed_series_names
#' @usage get_changed_series_names(x)
#' @param x istanza di Grafo
#' @param con eventuale connessione al database (se non presente, ne crea una)
#' @return lista di nomi di serie cambiate sul database rispetto ad X
#' @note funzione interna

get_changed_series_names <- function(x, con = NULL) {
  con <- if (is.null(con)) {
    con <- buildConnection()
    on.exit(disconnect(con))
    con
  } else {
    con
  }

  tag <- x@tag
  helper <- x@helper
  timestamp <- x@timestamp
  df <- DBI::dbGetQuery(con, getSQLbyKey(
    helper, "GET_CHANGED_SERIES", tag = tag,
    last_updated = timestamp))
  nomi <- as.character(df$name)
  unique(nomi)
}

get_outer_data_names <- function(x, con = NULL) {
  con <- if (is.null(con)) {
    con <- buildConnection()
    on.exit(disconnect(con))
    con
  } else {
    con
  }
  autore <- rutils::whoami()
  tag <- x@tag
  timestamp <- x@timestamp
  helper <- x@helper
  df <- DBI::dbGetQuery(con, getSQLbyKey(
    helper, "GET_OUTER_DATA_NAMES", tag = tag, last_updated = timestamp,
    autore = autore))
  as.character(df$name)
}

get_outer_formula_names <- function(x, con = NULL) {
  con <- if (is.null(con)) {
    con <- buildConnection()
    on.exit(disconnect(con))
    con
  } else {
    con
  }

  autore <- rutils::whoami()
  tag <- x@tag
  timestamp <- x@timestamp
  helper <- x@helper

  df <- DBI::dbGetQuery(con, getSQLbyKey(
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

  nameData <- hash::keys(x@data)
  namesFormule <- hash::keys(x@functions)

  outerDataNames <- get_outer_data_names(x, con = con)
  outerFormulaNames <- get_outer_formula_names(x, con = con)

  datiComuni <- intersect(nameData, outerDataNames)
  if (length(datiComuni) > 0) {
    # trovo solo le root
    soloRoots <- intersect(.roots(x), datiComuni)
    if (length(soloRoots) > 0) {
      # Controllo una ad una le radici e verifico differenze di valori
      dati.db <- loadDati(tag, con = con)
      for (name in unique(soloRoots)) {
        outerTs <- as.character(dati.db[dati.db$name == name, "dati"])
        innerTs <- as.character(to.data.frame(x[[name]])[1, "dati"])
        if ( outerTs != innerTs ) {
          create_data_conflicts(x, name, con = con)
        }
      }
    }
  }

  formuleComuni <- intersect(namesFormule, outerFormulaNames)
  if (length(formuleComuni) > 0) {
    #controllo ogni nome per verificare differenze.
    formule.db <- loadFormule(tag, con = con)
    formule.db <- formule.db[formule.db$name %in% formuleComuni, ]
    for (name in unique(as.character(formule.db$name))) {
      formula.db <- formule.db[formule.db$name == name, ]$formula
      if (formula.db != functions[[name]]) {
        ## crea conflitto su formule per name
        create_function_conflicts(x, name, formula.db, con = con)
      }
    }
  }

  x
}

#' @importFrom foreach %do%
#' @include db.r persistence_utils.r

create_data_conflicts <- function(x, nomi, con = NULL) {
  con <- if (is.null(con)) {
    con <- buildConnection()
    on.exit(disconnect(con))
    con
  } else {
    con
  }

  tag <- x@tag
  autore <- rutils::whoami()
  helper <- x@helper
  timestamp <- time.in.millis()
  dati <- foreach::foreach(name = iterators::iter(nomi), .combine = rbind) %do% {

    tt <- x[[name]]
    df <- to.data.frame(tt, name)
    anno <- df$anno
    prd <- df$periodo
    freq <- df$freq
    dati <- df$dati

    tryCatch({
      DBI::dbExecute(con, getSQLbyKey(
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
  }
  warning(
    "Ci sono conflitti sui dati per le serie: ",
    paste(nomi, collapse = ", "))
}

create_function_conflicts <- function(x, nomi, formula.db, con = NULL) {

  conWasNull <- is.null(con)
  con <- if (is.null(con)) {
    con <- buildConnection(con = con)
    on.exit(disconnect(con))
    con
  } else {
    con
  }

  autore <- rutils::whoami()
  tag <- x@tag
  helper <- x@helper

  timestamp <- time.in.millis()
  foreach::foreach(name = iterators::iter(nomi)) %do% {
    sql1 <- getSQLbyKey(
      helper, "CREA_CONFLITTO_FORMULE1",
      formula = formula.db,
      autore = autore,
      name = name,
      tag = tag,
      last_updated = timestamp)

    DBI::dbExecute(con, sql1)

    sql2 <- getSQLbyKey(
      helper, "CREA_CONFLITTO_FORMULE2",
      formula = formula.db,
      autore = autore,
      name = name,
      tag = tag,
      last_updated = timestamp)
    DBI::dbExecute(con, sql2)
  }

  warning(
    "Ci sono conflitti sulle formule per le serie: ",
    paste(nomi, collapse = ", "))
}
