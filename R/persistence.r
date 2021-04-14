
#' Funzione per salvare un grafo
#'
#' La funzione controlla la presenza di eventuali conflitti e necessita'
#' di risincronizzare i dati del DB con quelli presenti nel Grafo.
#'
#' \itemize{
#'   \item{"1"}{
#'      Identificare le serie aggregate (solo formule) - primitive (solo dati)
#'      cambiate, escludendo eventuali conflitti}
#'   \item{"2"}{Caricarle nel grafo}
#'   \item{"3"}{Rieseguirle}
#'   \item{"4"}{Risalvare il grafo}
#' }
#'
#' La funzione controlla se esistono conflitti nel seguente modo:
#' \itemize{
#'   \item{"dati"}{
#'        Se esistono serie primitive nel DB e nel grafo
#'        in sessione che sono state aggiornate in
#'        contemporanea}
#'   \item{"formule"}{
#'        Se esistono formule nel DB e nel grafo in
#'        sessione aggiornati in contemporanea}
#' }
#'
#' Qualora uno dei due casi si verificasse il grafo va in "conflitto",
#' vengono salvate sia le proprie modifiche che le modifiche fatte da
#' altri e si attende la risoluzione del conflitto attraverso i metodi
#' `fixConflict`. La soluzione dei conflitti non e' un atto di fede:
#' occorre incontrarsi e decidere quale "formula" o quale versione dei dati
#' sia da preferire.
#'
#' @seealso saveGraph
#' @name .saveGraph
#' @usage .saveGraph(x, tag)
#' @usage .saveGraph(x)
#' @include conflicts.r copy_graph.r assert_dag.r persistence_utils.r
#' @rdname saveGraph-internal
#' @note \url{https://osiride-public.utenze.bankit.it/group/894smf/trac/cfin/ticket/31849}
#' @include logging.r
# FIXME: https://osiride-public.utenze.bankit.it/group/894smf/trac/cfin/ticket/31849

.saveGraph <- function(x, tag = x@tag, ...) {
  ln <- "GrafoDB.persistence"
  trace(".saveGraph started", name=ln)

  ln <- "GrafoDB.persistence.saveGraph"
  trace(".saveGraph started", name=ln)

  param_list <- list(...)

  msg <- if('msg' %in% names(param_list)) {
    param_list[["msg"]]
  } else {
    ""
  }

  con <- if('con' %in% names(param_list)) {
    debug('connection context provided', name=ln)
    param_list[['con']]
  }  else {
    debug('connection has to be created...', name=ln)
    con <- build_connection()
    on.exit(disconnect(con))
    debug('connection created and set to be closed on.exit', name=ln)
    con
  }

  debug("Message used for saving: %s", msg, name=ln)

  tryCatch({
    trace("beginning transaction", name=ln)
    DBI::dbBegin(con)

    if(has_conflicts(x, con=con)) {
      stop("Il grafo ", tag, " ha conflitti, risolverli prima di salvare")
    }

    if (need_resync(x)) {
      info("Resync started", name=ln)
      # risincronizzo i dati del db con la copia nel grafo
      x <- resync(x, con=con)
      # trova serie che necessitano il resync
      name_to_sync <- get_changed_series_names(x, con=con)
      # trova serie con conflitti
      name_in_conflicts <- intersect(name_to_sync, union(hash::keys(x@functions), hash::keys(x@data)))
      clean_names <- setdiff(name_to_sync, name_in_conflicts)
      # clean_names contiene le serie che possono essere ricaricate dal db e rivalutate
      # senza problemi
      # aggiungo gli archi del DB al presente grafo
      network <- x@network
      archi <- load_edges(tag, con=con)
      archi <- archi[, c("partenza", "arrivo")]
      dbnetwork <- igraph::graph.data.frame(as.data.frame(archi), directed=TRUE)
      network <- igraph::graph.union(network, dbnetwork, byname=TRUE)
      assert_dag(network)
      x@network <- network
      x <- evaluate(x, clean_names)
    }

    check_conflicts(x, con=con)

    if(exists_tag(tag, con=con)) {
      # se esiste il tag sul DB
      # sto aggiornando il grafo tag
      trace("'%s' exists on DB, I'm updating it...", tag, name=ln)
      if(x@tag != tag) {
        trace("x@tag ('%s') != tag (%s), execute history, delete tag and recreate a copy of it",
                   x@tag, tag, name=ln)
        # faccio l'history del tag di destinazione
        do_history(x, tag, con)
        # lo cancello
        .delete_graph(tag, con, x@helper)
        # copio il grafo in sessione col grafo attuale
        copy_graph(x@tag, tag, con=con, mesg=msg, helper=x@helper)
      }
      # aggiorno eventuali cambiamenti in sessione
      trace("update eventual changes in session", name=ln)
      update_graph(x, con=con, msg=msg)
    } else {
      if (x@tag == tag) {
        trace('tag as param equals tag as slot: creating a new graph', name=ln)
        # se non esiste il tag sul DB
        # sto creando un nuovo grafo
        create_graph(x, tag, con=con, msg=msg)
      } else {
        # se i tag sono differenti
        if (nrow(x@dbdati) == 0 && nrow(x@dbformule) == 0) {
          trace('have no data, simply create an empty graph', name=ln)
          # non ho dati, creo grafo
          create_graph(x, tag, con=con, msg=msg)
        } else {
          # ho dati, quindi copio il grafo dalla fonte alla
          # destinazione sul DB e...
          trace('have data, so copying graph... ', name=ln)
          copy_graph(x@tag, tag, con=con, msg=msg, helper=x@helper)
          # Aggiorno eventuali cambiamenti in sessione
          trace('... and update eventual changes in session', name=ln)
          update_graph(x, tag, con=con, msg=msg)
        }
      }
    }

    DBI::dbCommit(con)
  }, error=function(err) {
    tryCatch({
      DBI::dbRollback(con)
    }, error = function(err2) {
      stop(err2, "Root: ", err)
    })
    stop(err)
  })
  x
}


#' @include update_archi.r update_data.r update_functions.r

update_graph <- function(x, tag = x@tag, con = NULL, msg = "") {
  helper <- x@helper
  ## supporto per history
  do_history(x, tag = tag, con = con)
  update_data(x, con = con, tag = tag, notes = msg)
  update_functions(x, con = con, tag = tag, msg = msg)
  update_edges(x, con = con, tag = tag)
  DBI::dbExecute(con, sql_by_key(
    helper, "UPDATE_GRAFO_LAST_UPDATED",
    autore=rutils::whoami(),
    tag=tag,
    last_updated=time.in.millis()))
}


#' crea ex-novo un istanza di grafo nel databae
#'
#' @name create_graph
#' @rdname create_graph-internal
#' @param x istanza di Grafo
#' @param tag identificativo della versione
#' @param con connessione al DB
#' @usage create_graph(g, tag)

create_graph <- function(x, tag, con, ...) {
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

  autore <- rutils::whoami()
  helper <- x@helper

  if(length(names(x))) {
    dati <- foreach::`%do%`(foreach::foreach (name = iterators::iter(names(x)), .combine=rbind), {
      tt <- x[[name]]
      df <-  to_data_frame(tt, name)
      anno <- as.numeric(df$anno)
      periodo <- as.numeric(df$periodo)
      freq <- as.numeric(df$freq)
      dati <- as.character(df$dati)

      DBI::dbExecute(con, sql_by_key(
        helper, "INSERT_DATI",
        tag=tag,
        name=name,
        anno=anno,
        periodo=periodo,
        freq=freq,
        dati=dati,
        autore=autore,
        last_updated=time.in.millis()))
    })
  } else {
    stop("Non ci sono dati da salvare.")
  }

  archi <- as.data.frame(igraph::get.edgelist(x@network))

  if(nrow(archi)) {
    foreach::`%do%`(foreach::foreach(row = iterators::iter(archi, 'row')), {
      partenza <- row[,1]
      arrivo <- row[,2]
      DBI::dbExecute(con, sql_by_key(
        helper, "INSERT_ARCO",
        tag=tag,
        partenza=partenza,
        arrivo=arrivo,
        autore=autore,
        last_updated=time.in.millis()))
    })
  }

  foreach::`%do%`(foreach::foreach(
    name = iterators::iter(names(x)), .combine=rbind), {
    formula <- expr(x, name, echo=FALSE)
    if(!is.null(formula)) {
      DBI::dbExecute(con, sql_by_key(
        helper, "INSERT_FORMULA",
        tag=tag,
        name=name,
        formula=formula,
        autore=autore,
        last_updated=time.in.millis()))
    }
  })

  DBI::dbExecute(con, sql_by_key(
    helper, "INSERT_GRAFI", 
    tag=tag,
    commento=commento,
    autore=autore,
    last_updated=time.in.millis()))
}


#' conta le versioni rolling del grafo con tag `tag`
#'
#' @name count_rolling
#' @usage count_rolling(x)
#' @param x istanza di grafo
#' @param con connessione al DB
#' @return un intero ad indicare il numero di versioni rolling salvate sul DB
#' @include db.r

count_rolling <- function(x, con) {
  stopifnot(is.grafodb(x))
  tag <- x@tag
 
  # controlla che grafi_`tag`_ordinal_seq esista.
  # se esiste, prende il prossimo `p` dalla sequence;
  # se non esiste, esegue il blocco qui sotto, crea la sequence e aggiorna il valore

  helper <- x@helper
  if (helper@type == "PostgreSQL") {
    ## se PostgreSQL:
    nome_seq <- paste0("grafi_", tag, "_ordinal_seq")

    if(nrow(DBI::dbGetQuery(con, sql_by_key(helper, "EXISTS_SEQ", seq=nome_seq))) > 0) {
      df <- DBI::dbGetQuery(con, sql_by_key(helper, "NEXT_VAL", seq=nome_seq))
      as.numeric(df[[1]])
    } else {
      val <- getMaxP(helper, tag, con) + 1
      DBI::dbExecute(con, sql_by_key(helper, "CREATE_SEQ", seq=nome_seq, val=val))
      count_rolling(x, con)
    }
  } else {
    ## se SQLite:
    getMaxP(helper, tag, con) + 1
  }
}

getMaxP <- function(helper, tag, con) {
  df <- DBI::dbGetQuery(con, sql_by_key(helper, "COUNT_ROLLING", tag=tag))
  if(nrow(df) == 0) {
    0
  } else {
    numeri <- suppressWarnings(as.numeric(gsub("p", "", gsub(tag, "", df[, 1]))))
    max(numeri, na.rm=TRUE)
  }
}


#' Costruice il progressivo per il grafo `x`
#'
#' @name next_rolling_name
#' @usage next_rolling_name(x)

next_rolling_name <- function(x, con) {
  tag <- x@tag
  p <- count_rolling(x, con) 
  paste0(tag, 'p', p)
}


#' Esegue il rolling dei vintage del `GrafoDB`
#'
#' Ad ogni salvataggio con il metodo `saveGraph` se non impostiamo
#' un nuovo `tag` il `GrafoDB` salva i dati sullo stesso `tag` ma
#' contemporaneamente salva la versione precedente con un progressivo,
#' in modo da tener traccia di chi ha fatto cosa nel tempo.
#'
#' Le versioni sono contraddistinte da un nuovo tag, `tag`p`X` dove
#' `X` e' un numero progressivo
#'
#' Il grafo potra' successivamente essere caricato con il nuovo tag.
#'
#' @name do_history
#' @usage do_history(x, con)
#' @param x istanza di `GrafoDB`
#' @param con connessione al database
#' @note questa e' una funzione interna del grafo invocata da `update_graph`
#' @seealso saveGraph update_graph

do_history <- function(x, tag, con) {
  ril <- rilasci(tag)
  autore <- ril[ril$tag == x@tag, ]$autore
  if(length(autore) == 0) {
    autore <- rutils::whoami()
  }

  dest <- next_rolling_name(x, con)
  if(interactive()) message("Saving GrafoDB from ", x@tag, " to ", dest)
  copy_graph(x@tag, dest, con=con, autore=autore,
            helper=x@helper, last_update=x@timestamp)
  if(interactive()) message("Saving ", dest, " completed")
  0
}

#' Salva un istanza di grafo sul file system 
#'
#' @name saveBinary
#' @usage saveBinary(x, path)
#' @param x istanza del GrafoDB
#' @param path percorso del file su cui salvare il grafo
#' @export
#' @note il restore si fa con il comando `readBinary`

saveBinary <- function(x, path) {
  con <- file(path, "wb")
  on.exit(close(con))
  ret <- serialize(x, con, ascii = TRUE)
  invisible(ret)
}


#' Legge un GrafoDB dal filesystem in formato binario con `saveBinary`
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
