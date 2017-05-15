
#' funzione per salvare un grafo
#'
#' @name .saveGraph
#' @usage .saveGraph(x, tag)
#' @usage .saveGraph(x)
#' @include conflicts.r copy_graph.r
#' @rdname saveGraph-internal

# FIXME: #31849
# https://osiride-public.utenze.bankit.it/group/894smf/trac/cfin/ticket/31849

.saveGraph <- function(x, tag = x@tag, ...) {

  con <- pgConnect()
  on.exit(dbDisconnect(con))

  param_list <- list(...)

  msg <- if('msg' %in% names(param_list)) {
    param_list[["msg"]]
  } else {
    ""
  }

  tryCatch({
    dbBegin(con)
    if(hasConflicts(x, con=con)) {
      stop("Il grafo ", tag, " ha conflitti, risolverli prima di salvare")
    }
    checkConflicts(x, con=con)
    if(.tagExists(tag, con=con)) {
      # sto aggiornando il grafo tag
      .updateGraph(x, con=con, msg=msg)
    } else {
      if (x@tag == tag) {
        # sto creando un nuovo grafo
        .createGraph(x, tag, con=con, msg=msg)  
      } else {
        if (nrow(x@dbdati) == 0 && nrow(x@dbformule) == 0) {
          .createGraph(x, tag, con=con, msg=msg)
        } else {
          .copyGraph(x@tag, tag, con=con, msg=msg)
          .updateGraph(x, tag, con=con, msg=msg)
        }
      }
    }
    removeFromRedis(x, x@touched)
    dbCommit(con)
  }, error=function(err) {
    tryCatch({  
      dbRollback(con)
    }, error = function(err2) {
      stop(err2, "Root: ", err)
    })
    stop(err)
  })
  x
}

#' @include update_archi.r update_data.r update_functions.r
#' @importFrom R.utils System

.updateGraph <- function(x, tag=x@tag, con=NULL, msg="") {
  helper <- x@helper
  ## supporto per history
  doHistory(x, con=con)
  .updateData(x, con=con, tag=tag, msg=msg)
  .updateFunctions(x, con=con, tag=tag, msg=msg)
  .updateArchi(x, con=con, tag=tag)
  dbGetQuery(con, getSQLbyKey(
    helper, "UPDATE_GRAFO_LAST_UPDATED",
    autore=whoami(),
    tag=tag,
    last_updated=round(R.utils::System$currentTimeMillis())))
}

#' crea ex-novo un istanza di grafo nel databae
#'
#' @name .createGraph
#' @rdname createGraph-internal
#' @param x istanza di Grafo
#' @param tag identificativo della versione
#' @param con connessione al DB
#' @usage .createGraph(g, tag)
#' @importFrom foreach foreach %do%
#' @importFrom rutils whoami
#' @importFrom DBI dbSendQuery dbRollback
#' @importFrom R.utils System

.createGraph <- function(x, tag, con, ...) {
  param_list <- list(...)
  commento <- if ("msg" %in% names(param_list)) {
    param_list[["msg"]]
  } else {
    if(interactive()) {
      readline(prompt="Inserisci un commento/nota per: ")
    } else {
      paste0("Rilascio per ", tag)
    }
  }
  
  autore <- whoami()
  helper <- x@helper
  
  if(length(names(x))) {
    dati <- foreach (name = iter(names(x)), .combine=rbind) %do% {
      tt <- x[[name]]
      df <- to.data.frame(tt, name)
      anno <- df$anno
      periodo <- df$periodo
      freq <- df$freq
      dati <- df$dati
      
      dbGetQuery(con, getSQLbyKey(
        helper, "INSERT_DATI",
        tag=tag,
        name=name,
        anno=anno,
        periodo=periodo,
        freq=freq,
        dati=dati,
        autore=autore,
        last_updated=round(R.utils::System$currentTimeMillis())))
    }
  } else {
    stop("Non ci sono dati da salvare.")
  }
  
  archi <- as.data.frame(get.edgelist(x@network))
  
  if(nrow(archi)) {
    foreach(row = iter(archi, 'row')) %do% {
      partenza <- row[,1]
      arrivo <- row[,2]
      dbGetQuery(con, getSQLbyKey(
        helper, "INSERT_ARCO",
        tag=tag,
        partenza=partenza,
        arrivo=arrivo,
        autore=autore,
        last_updated=round(R.utils::System$currentTimeMillis())))
    }
  }
  
  foreach(name = iter(names(x)), .combine=rbind) %do% {
    formula <- expr(x, name, echo=FALSE)
    if(!is.null(formula)) {
      dbGetQuery(con, getSQLbyKey(
        helper, "INSERT_FORMULA",
        tag=tag,
        name=name,
        formula=formula,
        autore=autore,
        last_updated=round(R.utils::System$currentTimeMillis())))
    }
  }
  
  dbGetQuery(con, getSQLbyKey(
    helper, "INSERT_GRAFI", 
    tag=tag,
    commento=commento,
    autore=autore,
    last_updated=round(R.utils::System$currentTimeMillis())))
}


#' conta le versioni rolling del grafo con tag `tag`
#'
#' @name countRolling
#' @usage countRolling(x)
#' @param x istanza di grafo
#' @param con connessione al DB
#' @return un intero ad indicare il numero di versioni rolling salvate sul DB
#' @importFrom DBI dbGetQuery
#' @include db.r

countRolling <- function(x, con) {
  stopifnot(is.grafodb(x))
  tag <- x@tag
 
  # controlla che grafi_`tag`_ordinal_seq esista.
  # se esiste, prende il prossimo `p` dalla sequence;
  # se non esiste, esegue il blocco qui sotto, crea la sequence e aggiorna il valore

  helper <- x@helper
  if (helper@type == "PostgreSQL") {
    ## se PostgreSQL:
    nome_seq <- paste0("grafi_", tag, "_ordinal_seq")
    
    if(nrow(dbGetQuery(con, getSQLbyKey(helper, "EXISTS_SEQ", seq=nome_seq))) > 0) {
      df <- dbGetQuery(con, getSQLbyKey(helper, "NEXT_VAL", seq=nome_seq))
      as.numeric(df[[1]])
    } else {
      val <- getMaxP(helper, tag, con) + 1
      dbGetQuery(con, getSQLbyKey(helper, "CREATE_SEQ", seq=nome_seq, val=val))
      countRolling(x, con)
    }
  } else {
    ## se SQLite:
    getMaxP(helper, tag, con)
  }
}

getMaxP <- function(helper, tag, con) {
  df <- dbGetQuery(con, getSQLbyKey(helper, "COUNT_ROLLING", tag=tag))
  if(nrow(df) == 0) {
    0
  } else {
    numeri <- suppressWarnings(as.numeric(gsub("p", "", gsub(tag, "", df[, 1]))))
    max(numeri, na.rm=TRUE)
  }
}
  


#' Costruice il progressivo per il grafo `x`
#'
#' @name nextRollingNameFor
#' @usage nextRollingNameFor(x)

nextRollingNameFor <- function(x, con) {
  tag <- x@tag
  p <- countRolling(x, con) + 1
  paste0(tag, 'p', p)
}

#' Esegue il rolling dei vintage del `GrafoDB`
#'
#' Ad ogni salvataggio con il metodo `saveGraph` se non impostiamo un nuovo `tag`
#' il `GrafoDB` salva i dati sullo stesso `tag` ma contemporaneamente salva la versione
#' precedente con un progressivo, in modo da tener traccia di chi ha fatto cosa nel tempo.
#'
#' Le versioni sono contraddistinte da un nuovo tag, `tag`p`X` dove `X` e' un numero progressivo
#'
#' Il grafo potra' successivamente essere caricato con il nuovo tag.
#'
#' @name doHistory
#' @usage doHistory(x, con)
#' @param x istanza di `GrafoDB`
#' @param con connessione al database
#' @note questa e' una funzione interna del grafo invocata da `updateGraph`
#' @seealso saveGraph updateGraph
#' @importFrom DBI dbGetQuery
#' @importFrom rprogressbar ProgressBar updateProgressBar kill
#' @importFrom RJSONIO toJSON
#' @importFrom iterators iter
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom rutils slice

doHistory <- function(x, con) {
  notOk <- TRUE
  tries <- 3
  ril <- rilasci(x@tag)
  autore <- ril[ril$tag == x@tag, ]$autore
  while(tries > 0) {    
    tries <- tryCatch({
      dest <- nextRollingNameFor(x, con)
      if(interactive()) message("Salvo il grafo ", x@tag, " in ", dest)
      .copyGraph(x@tag, dest, con=con, autore=autore)
      if(interactive()) message("salvataggio ", dest, " completo")
      0
    }, error=function(cond) {
      warning(cond)
      if (interactive()) message("Ritento il salvataggio...")
      if((tries - 1) == 0) {
        stop(cond)
      }
    })
  }  
}

#' Salva un istanza di grafo sul file system
#'
#' @name saveBinary
#' @usage saveBinary(x, path)
#' @param x istanza del GrafoDB
#' @param path percorso del file su cui salvare il grafo
#' @export

saveBinary <- function(x, path) {
  con <- file(path, "wb")
  on.exit(close(con))
  
  ret <- serialize(x, con, ascii = TRUE)
  invisible(ret)
}


#' Legge un GrafoDB dal filesystem in formato binario
#'
#' @name readBinary
#' @usage readBinary(path)
#' @param path percorso del file con il GrafoDB
#' @return GrafoDB contenuto nel file `path`
#' @export

readBinary <- function(path) {
  con <- file(path, "rw")
  on.exit(close(con))
  unserialize(con)
}
