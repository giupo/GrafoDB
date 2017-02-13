
.hasConflicts <- function(x, name=NULL, con=NULL) {
  nrow(getConflicts(x, name=name, con=con)) > 0
}


.removeConflicts <- function(x, name=NULL) {
  tag <- x@tag
  helper <- x@helper
  key <- if(is.character(name)) {
    "REMOVE_CONFLICTS_NAME"
  } else {
    "REMOVE_CONFLICTS"
  }
  
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  tryCatch({
    dbBegin(con)
    dbGetQuery(con, getSQLbyKey(helper, key, tag=tag, name=name))
    dbCommit(con)
  }, error = function(err) {
    dbRollback(con)
    stop(err)
  })
}

#' Ritorna `TRUE` se il grafo ha conflitti
#'
#' @name hasConflicts
#' @usage hasConflicts(x)
#' @usage hasConflicts(x, name)
#' @param x oggetto R
#' @param name character array di nomi (puo' essere omesso)
#' @return `TRUE` se l'istanza `x` e' un GrafoDB con conflitti, `FALSE` altrimenti
#' @examples \dontrun{
#' g <- GrafoDB(...)
#' hasConflicts(g) # dovrebbe essere FALSE
#' ...             # eseguo operazioni come modificare la stessa serie
#' hasConflicts(g) # TRUE
#' hasConflicts(g, SERIE_CON_CONFLITTO) # TRUE
#' hasConflicts(g, SERIE_SENZA_CONFLITTO) # FALSE
#' }
#' @export
#' @include core.r

setGeneric(
  "hasConflicts",
  function(x, name=NULL, con=NULL) {
    standardGeneric("hasConflicts")
  })

#' Ritorna `TRUE` se il grafo ha conflitti
#'
#' @name hasConflicts
#' @usage hasConflicts(x)
#' @usage hasConflicts(x, name)
#' @param x oggetto R
#' @param name character array di nomi (puo' essere omesso)
#' @return `TRUE` se l'istanza `x` e' un GrafoDB con conflitti, `FALSE` altrimenti
#' @examples \dontrun{
#' g <- GrafoDB(...)
#' hasConflicts(g) # dovrebbe essere FALSE
#' ...             # eseguo operazioni come modificare la stessa serie
#' hasConflicts(g) # TRUE
#' hasConflicts(g, SERIE_CON_CONFLITTO) # TRUE
#' hasConflicts(g, SERIE_SENZA_CONFLITTO) # FALSE
#' }
#' @export
#' @include db.r

setMethod(
  "hasConflicts",
  signature("GrafoDB", "ANY"),
  function(x, name=NULL, con=NULL) {
    .hasConflicts(x, name=name, con=con)
  })


#' Ritorna i conflitti per un Grafo
#'
#' Ritorna i conflitti di un grafo, tutti per serie specificate dal parametro `name`
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
  function(x, name=NULL, con=NULL) {
    standardGeneric("getConflicts")
  })

setMethod(
  "getConflicts",
  signature("GrafoDB", "ANY"),
  function(x, name=NULL, con=NULL) {
    if (is.null(con)) {
      on.exit(dbDisconnect(con))
    }
    con <- pgConnect(con)
    
    tag <- x@tag
    helper <- x@helper
    sql <- if(is.null(name)) {
      getSQLbyKey(helper, "GET_CONFLICTS", tag=tag)
    } else {
      getSQLbyKey(helper, "GET_CONFLICTS_NAME", tag=tag, name=name)
    }

    dbGetQuery(con, sql)
  })


setGeneric(
  "getDataConflicts",
  function(x, name=NULL) {
    standardGeneric("getDataConflicts")
  })

setMethod(
  "getDataConflicts",
  signature("GrafoDB", "ANY"),
  function(x, name=NULL) {
    df <- getConflicts(x, name)
    if(nrow(df)) {
      ret <- list()
      lista <- as.character(df$name)
      foreach(name=iter(lista), .combine=rbind) %do% {
        autore <- df[df$name == name, "autore"]
        current_autore <- df[df$name == name, "old_autore"]
        current <- df[df$name == name, c("name", "old_anno", "old_periodo", "old_freq", "old_dati")]
        colnames(current) <- c("name", "anno", "periodo", "freq", "dati")
        current <- from.data.frame(current)[[name]]
        
        nuova <- df[df$name == name, c("name", "anno", "periodo", "freq", "dati")]
        colnames(nuova) <- c("name", "anno", "periodo", "freq", "dati")
        nuova <- from.data.frame(nuova)[[name]]
        
        differenza <- if(frequency(nuova) == frequency(current)) {
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
  function(x, name=NULL) {
    standardGeneric("getFormulaConflicts")
  })

setMethod(
  "getFormulaConflicts",
  signature("GrafoDB", "ANY"),
  function(x, name=NULL) {
    tag <- x@tag
    helper <- x@helper
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    sql <- if(is.null(name)) {
      getSQLbyKey(helper, "GET_FORMULA_CONFLICTS", tag=tag)
    } else {
      getSQLbyKey(helper, "GET_FORMULA_CONFLICTS_NAME", tag=tag, name=name)
    }
    dbGetQuery(con, sql)
  })


#' contrassegna come risolti i conflitti salvati sul DB
#'
#' E' appannaggio dell'utente riusolvere i conflitti nella sua sessione e provvedere
#' a salvate un Grafo consistente.
#'
#' @name fixConflicts
#' @usage fixConflicts(x)
#' @usage fixConflicts(x, name)
#' @param x istanza di GrafoDB
#' @param name nome della serie da cui eliminare un conflitto
#' @export

setGeneric(
  "fixConflicts",
  function(x, name=NULL) {
    standardGeneric("fixConflicts")
  })

setMethod(
  "fixConflicts",
  signature("GrafoDB"),
  function(x, name=NULL) {
    .removeConflicts(x, name=name)
  })

#' Trova le serie che sono cambiate nel database
#'
#' @name getChangedSeries
#' @usage getChangedSeries(x)
#' @param x istanza di Grafo
#' @param con eventuale connessione al database (se non presente, ne crea una)
#' @return lista di nomi di serie cambiate sul database rispetto ad X
#' @note funzione interna

getChangedSeries <- function(x, con=NULL) {
  if(is.null(con)) {
    on.exit(dbDisconnect(con))
  }
  con <- pgConnect(con=con)
  tag <- x@tag
  helper <- x@helper
  timestamp <- x@timestamp
  df <- dbGetQuery(con, getSQLbyKey(
    helper, "GET_CHANGED_SERIES", tag=tag,
    last_updated=timestamp))
  nomi <- as.character(df$name)
  unique(nomi)
}

getOuterDataNames <- function(x, con=NULL) {
  conWasNull <- is.null(con)
  con <- pgConnect(con=con)
  if(conWasNull) {
    on.exit(dbDisconnect(con))
  }
  autore <- whoami()
  tag <- x@tag
  timestamp <- x@timestamp
  helper <- x@helper
  df <- dbGetQuery(con, getSQLbyKey(
    helper, "GET_OUTER_DATA_NAMES", tag=tag, last_updated=timestamp,
    autore=autore))
  as.character(df$name)
}

getOuterFormulaNames <- function(x, con=NULL) {
  conWasNull <- is.null(con)
  con <- pgConnect(con=con)
  if(conWasNull) {
    on.exit(dbDisconnect(con))
  }

  autore <- whoami()
  tag <- x@tag
  timestamp <- x@timestamp
  helper <- x@helper

  df <- dbGetQuery(con, getSQLbyKey(
    helper, "GET_OUTER_FORMULA_NAMES", tag=tag, last_updated=timestamp,
    autore=autore))
  as.character(df$name)
}

#' @include functions.r
#' @importFrom hash keys
#' @include db.r persistence_utils.r

checkConflicts <- function(x, con=NULL) {
   
  tag <- x@tag

  data <- x@data
  functions <- x@functions
  
  nameData <- keys(x@data)
  namesFormule <- keys(x@functions)

  outerDataNames <- getOuterDataNames(x, con=con)
  outerFormulaNames <- getOuterFormulaNames(x, con=con)

  datiComuni <- intersect(nameData, outerDataNames)
  if(length(datiComuni) > 0) {
    # trovo solo le root
    soloRoots <- intersect(.roots(x), datiComuni)
    if(length(soloRoots) > 0) {   
      # Controllo una ad una le radici e verifico differenze di valori
      dati.db <- loadDati(tag, con=con)
      for(name in unique(soloRoots)) {
        outerTs <- from.data.frame(dati.db[dati.db$name == name, ])[[name]]
        innerTs <- x[[name]]
        if(any(outerTs != innerTs)) {
          creaConflittoDati(x, name, con=con)
        }
      }
    }
  }
  
  formuleComuni <- intersect(namesFormule, outerFormulaNames)
  if(length(formuleComuni) > 0) {  
    #controllo ogni nome per verificare differenze.
    formule.db <- loadFormule(tag, con=con)
    formule.db <- formule.db[formule.db$name %in% formuleComuni,]
    for(name in unique(as.character(formule.db$name))) {
      formula.db <- formule.db[formule.db$name == name,]$formula
      if(formula.db != functions[[name]]) {
        ## crea conflitto su formule per name
        creaConflittoFormule(x, name, con=con)
      }
    }
  }
  
  x
}

#' @importFrom rutils whoami
#' @importFrom foreach foreach %do%
#' @importFrom iterators iter
#' @importFrom DBI dbDisconnect
#' @include db.r

creaConflittoDati <- function(x, nomi, con=NULL) {
  conWasNull <- is.null(con)
  con <- pgConnect(con=con)
  if(conWasNull) {
    on.exit(dbDisconnect(con))
  }
  
  tag <- x@tag
  autore <- whoami()
  helper <- x@helper
  timestamp <- round(R.utils::System$currentTimeMillis())
  dati <- foreach(name = iter(nomi), .combine=rbind) %do% {

    tt <- x[[name]]
    df <- to.data.frame(tt, name)
    anno <- df$anno
    prd <- df$periodo
    freq <- df$freq
    dati <- df$dati
  
    tryCatch({
      dbGetQuery(con, getSQLbyKey(
        helper, "CREA_CONFLITTO_DATI",
        tag=tag,
        name=name,
        anno=anno,
        periodo=prd,
        freq=freq,
        dati=dati,
        autore=autore,
        last_updated=timestamp))
    }, error = function(cond) {
      stop(cond)
    })
  }
  warning(
    "Ci sono conflitti sui dati per le serie: ",
    paste(nomi, collapse=", "))
}

creaConflittoFormule <- function(x, nomi, con=NULL) {
  
  conWasNull <- is.null(con)
  con <- pgConnect(con=con)
  if(conWasNull) {
    on.exit(dbDisconnect(con))
  }

  autore <- whoami()
  tag <- x@tag
  helper <- x@helper
  timestamp <- round(R.utils::System$currentTimeMillis())
  foreach (name = iter(nomi)) %do% {
    task <- expr(x, name, echo=FALSE)
    
    sql1 <- getSQLbyKey(
      helper, "CREA_CONFLITTO_FORMULE1",
      formula=task,
      autore=autore,
      name=name,
      tag=tag,
      last_updated=timestamp)
    
    df <- dbGetQuery(con, sql1)

    sql2 <- getSQLbyKey(
      helper, "CREA_CONFLITTO_FORMULE2",
      formula=task,
      autore=autore,
      name=name,
      tag=tag,
      last_updated=timestamp)
    dbGetQuery(con, sql2)
  }

  warning(
    "Ci sono conflitti sulle formule per le serie: ",
    paste(nomi, collapse=", "))
}
