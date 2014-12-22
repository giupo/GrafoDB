
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
#' @import rcf
#' @include core.r

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
