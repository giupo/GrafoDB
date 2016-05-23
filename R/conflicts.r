
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
#' @include db.r

setMethod(
  "hasConflicts",
  signature("GrafoDB", "ANY"),
  function(x, name=NULL) {
    df <- getConflicts(x, name=name)
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
    sql <- if(is.null(name)) {
      paste0("select a.name, a.tag, a.anno as anno, a.prd as periodo, a.freq as freq, ",
             " a.dati as dati, a.autore as autore,",
             " b.anno as old_anno, b.periodo as old_periodo, b.freq as old_freq , ",
             " b.dati as old_dati, b.autore as old_autore,",
             " date, last_updated, formula ",
             " from conflitti a, dati b ",
             " where a.tag = '", tag, "' and a.tag = b.tag and a.name = b.name",
             " order by tag, name")
    } else {
      paste0("select a.name, a.tag, a.anno, a.prd as periodo, a.freq, a.dati, a.autore,",
             "b.anno as old_anno, b.periodo as old_periodo, b.freq as old_freq , ",
             " b.dati as old_dati, b.autore as old_autore,",
             "date, formula ",
             "from conflitti a, dati b ",
             "where a.tag = '", tag, "' and a.name='", name, "' and a.tag = b.tag ",
             "and a.name = b.name order by tag, name")
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
    tag <- x@tag
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    sql <- if(is.null(name)) {
      paste0("select a.name, a.tag, a.formula, a.autore, date, ",
             " b.formula as current_formula, b.autore as current_autore, b.last_updated",
             " from conflitti a, formule b ",
             " where a.tag = '", tag, "' and a.tag = b.tag and a.name = b.name",
             " order by tag, name")
    } else {
      paste0("select a.name, a.tag, a.formula, a.autore, date, b.formula, b.autore, b.last_updated",
             " from conflitti a, formule b ",
             " where a.tag = '", tag, "' and a.name='", name, "' and a.tag = b.tag and a.name = b.name",
             " order by tag, name")
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
    params <- x@tag
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    sql <- if(is.null(name)) {
      paste0("delete from conflitti where tag = '", tag, "'")
    } else {
      paste0("delete from conflitti where tag = '", tag, "' and name = '", name, "'")
    }
    dbGetQuery(con, sql)
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
  con <- pgConnect(con=con)
  tag <- x@tag
  timestamp <- x@timestamp
  sql <- paste0("select name from dati where tag='", tag,
                "' and last_updated > '", timestamp, "'",
                "union",
                " select name from formule where tag='", tag,
                "' and last_updated > '", timestamp, "'")
  df <- dbGetQuery(con, sql)
  nomi <- as.character(df$name)
  unique(nomi)
}

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
    tryCatch({
      dbBegin(con)
    }, error = function(cond) {
      dbRollback(con)
      stop(cond)
    })
  }
  
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
      print(formula.db)
      print(functions)
      if(formula.db != functions[[name]]) {
        ## crea conflitto su formule per name
        creaConflittoFormule(x, name, con=con)
      }
    }
  }
  
  tryCatch({
    dbCommit(con)
  }, error = function(cond) {
    dbRollback(con)
    stop(cond)
  })

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
  sql <- paste0(
    "insert into conflitti(tag, name, anno, prd, ",
    " freq, dati, autore)",
    " values (?, ?, ?, ?, ?, ? ,?)")

  tag <- x@tag
  autore <- whoami()

  dati <- foreach(name = iter(nomi), .combine=rbind) %do% {

    tt <- x[[name]]
    df <- to.data.frame(tt, name)
    anno <- df$anno
    prd <- df$periodo
    freq <- df$freq
    dati <- df$dati
    
    sql <- paste0(
      "insert into conflitti(tag, name, anno, prd, ",
      " freq, dati, autore)",
      " values ('", tag,"', '", name, "', ", anno,", ", prd ,
      ", ", freq,", '", dati , "' ,'", autore, "')")
    
    
    # dati <- as.data.frame(dati)
    # names(dati) <- c("tag", names(df), "autore")
    tryCatch({
      dbGetQuery(con, sql)
    }, error = function(cond) {
      dbRollback(con)
      stop(cond)
    })
  }
  warning("Ci sono conflitti sui dati per le serie: ",
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
  
  foreach (name = iter(nomi)) %do% {
    task <- expr(x, name, echo=FALSE)
    
    sql1 <- paste0(
      "UPDATE conflitti  SET formula='", task, "', autore='", autore, "', ",
      "date = LOCALTIMESTAMP::timestamp(0) ",
      " WHERE name='", name, "' and tag='", tag, "'")
    
    dbGetQuery(con, sql1)
    
    
    sql2 <- paste0(
      "INSERT INTO conflitti(formula, autore, date, name, tag) ",
      " select '", task, "', '", autore, "',LOCALTIMESTAMP::timestamp(0),'", name,"','", tag, "'",
      " WHERE NOT EXISTS (SELECT 1 FROM formule WHERE name='", name, "' and tag='", tag, "')")
    
    dbGetQuery(con, sql2)
  }
  
  warning(
    "Ci sono conflitti sulle formule per le serie: ",
    paste(nomi, collapse=", "))

}
