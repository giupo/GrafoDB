
#' @importFrom DBI dbGetQuery
#' @importFrom RPostgreSQL dbGetQuery
#' @include db.r 

.showConflicts <- function(x) {
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  tag <- x@tag
  df <- dbGetQuery(
    con,
    paste0("select * from conflitti where tag = '", tag,"'"))

  for(name in df$name) {
    dfserie <- df[df$name == name,]
    originale <- x[[name]]
    sul.db <- from.data.frame(dfserie)
  }
}

.removeConflicts <- function(x, name=NULL) {
  tag <- x@tag
  
  sql <- if(is.character(name)) {
    paste0("delete from conflitti where tag = '", tag,"' and name ='", name, "'")
  } else {
    paste0("delete from conflitti where tag = '", tag, "'")
  }
  
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  tryCatch({
    dbBegin(con)
    dbGetQuery(con, sql)
  }, error = function(err) {
    dbRollback(con)
    stop(err)
  })
  dbCommit(con)
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
  function(x, name=NULL) {
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
#' @importFrom RPostgreSQL2 dbGetPreparedQuery
#' @include db.r

setMethod(
  "hasConflicts",
  signature("GrafoDB", "ANY"),
  function(x, name=NULL) {
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    tag <- x@tag
    df <- if(is.character(name)) {
      dbGetPreparedQuery(
        con,
        "select name from conflitti where tag = ? and name = ?",
        bind.data=cbind(tag, name))
    } else {
      dbGetPreparedQuery(
        con,
        "select name from conflitti where tag = ?",
        bind.data=tag)
    }

    nrow(df) > 0
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
  function(x, name=NULL) {
    standardGeneric("getConflicts")
  })

setMethod(
  "getConflicts",
  signature("GrafoDB", "ANY"),
  function(x, name=NULL) {
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    tag <- x@tag
    params <- as.data.frame(list(tag=tag), stringAsFactors=F)
    sql <- if(is.null(name)) {
      paste0("select a.name, a.tag, a.anno as anno, a.prd as periodo, a.freq as freq, ",
             " a.dati as dati, a.autore as autore,",
             " b.anno as old_anno, b.periodo as old_periodo, b.freq as old_freq , ",
             " b.dati as old_dati, b.autore as old_autore,",
             " date, last_updated, formula ",
             " from conflitti a, dati b ",
             " where a.tag = ? and a.tag = b.tag and a.name = b.name",
             " order by tag, name")
    } else {
      params <- cbind(params, name)
      names(params) <- c("tag", "name")
      paste0("select a.name, a.tag, a.anno, a.prd as periodo, a.freq, a.dati, a.autore,",
             "b.anno as old_anno, b.periodo as old_periodo, b.freq as old_freq , b.dati as old_dati, b.autore as old_autore,",
             "date, formula ",
             "from conflitti a, dati b ",
             "where a.tag = ? and a.name=? and a.tag = b.tag and a.name = b.name order by tag, name")
    }
    dbGetPreparedQuery(con, sql, bind.data = params)
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
        
        differenza <- if(TSINFO(nuova, MODE="FREQ") == TSINFO(current, MODE="FREQ")){
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
    params <- x@tag
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    sql <- if(is.null(name)) {
      paste0("select a.name, a.tag, a.formula, a.autore, date, ",
             " b.formula as current_formula, b.autore as current_autore, b.last_updated",
             " from conflitti a, formule b ",
             " where a.tag = ? and a.tag = b.tag and a.name = b.name",
             " order by tag, name")
    } else {
      params <- cbind(params, name)
      names(params) <- c("tag", "name")
      paste0("select a.name, a.tag, a.formula, a.autore, date, b.formula, b.autore, b.last_updated",
             " from conflitti a, formule b ",
             " where a.tag = ? and a.name=? and a.tag = b.tag and a.name = b.name",
             " order by tag, name")
    }
    dbGetPreparedQuery(con, sql, bind.data = params)
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
    params <- x@tag
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    sql <- if(is.null(name)) {
      ##paste0("select a.name, a.tag, a.formula, a.autore, date, ",
      ##       " b.formula as current_formula, b.autore as current_autore, b.last_updated",
      ##       " from conflitti a, formule b ",
      ##       " where a.tag = ? and a.tag = b.tag and a.name = b.name",
      ##       " order by tag, name")

      paste0("delete from conflitti where tag = ?")
    } else {
      params <- cbind(params, name)
      ##names(params) <- c("tag", "name")
      ##paste0("select a.name, a.tag, a.formula, a.autore, date, b.formula, b.autore, b.last_updated",
      ##       " from conflitti a, formule b ",
      ##       " where a.tag = ? and a.name=? and a.tag = b.tag and a.name = b.name",
      ##       " order by tag, name")
      paste0("delete from conflitti where tag = ? and name = ?")
    }
    dbGetPreparedQuery(con, sql, bind.data = params)
  })

getOuterDataNames <- function(x, con=NULL) {
  conWasNull <- is.null(con)
  con <- pgConnect(con=con)
  if(conWasNull) {
    on.exit(dbDisconnect(con))
  }
  
  tag <- x@tag
  timestamp <- x@timestamp
  sql <- paste0("select name from dati where tag='", tag,
                "' and last_updated > '", timestamp,"'")
  df <- dbGetQuery(con, sql)
  as.character(df$name)
}

getOuterFormulaNames <- function(x, con=NULL) {
  conWasNull <- is.null(con)
  con <- pgConnect(con=con)
  if(conWasNull) {
    on.exit(dbDisconnect(con))
  }
  
  tag <- x@tag
  timestamp <- x@timestamp
  sql <- paste0("select name from formule where tag='", tag,
                "' and last_updated > '", timestamp,"'")
  df <- dbGetQuery(con, sql)
  as.character(df$name)
}

#' @include functions.r
#' @importFrom hash keys
#' @include db.r persistence_utils.r

checkConflicts <- function(x, con=NULL) {
  conWasNull <- is.null(con)
  con <- pgConnect(con=con)
  if(conWasNull) {
    on.exit(dbDisconnect(con))
  }

  tag <- x@tag

  nameData <- keys(x@data)
  namesFormule <- keys(x@functions)

  outerDataNames <- getOuterDataNames(x, con=con)
  outerFormulaNames <- getOuterFormulaNames(x, con=con)

  datiComuni <- intersect(namesData, outerDataNames)
  if(length(datiComuni) > 0) {
    # trovo solo le root
    soloRoots <- intersect(.roots(x), datiComuni)
    if(length(soloRoots) > 0) {
      # Controllo una ad una le radici e verifico differenze di valori
      dati.db <- loadDati(tag, con=con)
      for(name in soloRoots) {
        outerTs <- from.data.frame(dati.db[dati.db$name == name, ])
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
    for(name in as.character(formule.db$name)) {
      formula.db <- formule.db[formule.db == name,]$formula
      if(formula.db != functions[[name]]) {
        ## crea conflitto su formule per name
        creaConflittoFormule(x, name, con=con)
      }
    }
  }
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

  sql <- paste0(
    "insert into conflitti(tag, name, anno, prd, ",
    " freq, dati, autore)",
    " values (?, ?, ?, ?, ?, ? ,?)")
  autore <- whoami()
  dati <- foreach(name = iter(nomi), .combine=rbind) %do% {
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
          paste(nomi, collapse=", "))
}

creaConflittoFormule <- function(x, nomi, con=NULL) {
  conWasNull <- is.null(con)
  con <- pgConnect(con=con)
  if(conWasNull) {
    on.exit(dbDisconnect(con))
  }
  
  dati <- foreach (name = iter(names.with.conflicts), .combine=rbind) %do% {
    task <- expr(x, name, echo=FALSE)
    cbind(task, autore, name, tag)
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
