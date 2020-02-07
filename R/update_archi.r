
#' @importFrom futile.logger flog.info
#' @include checkDAG.r

.updateArchi <- function(x, con, tag=x@tag) {
  ln <- "GrafoDB::updateArchi"
  
  if(interactive()) flog.info("Update Archi ...", name=ln)

  data <- x@data
  functions <- x@functions
  timestamp <- x@timestamp
  helper <- x@helper
  network <- x@network
  in.memory <- as.data.frame(igraph::get.edgelist(network), stringsAsFactors = F)
  autore <- rutils::whoami()
  names(in.memory) <- c("partenza", "arrivo")
  in.db <- dbGetQuery(con, getSQLbyKey(helper, "ARCHI_TAG", tag=tag))
  
  sep <- "-"
  in.db <- gdata::drop.levels(in.db)
  in.db <- paste(in.db$partenza, in.db$arrivo, sep=sep)
  in.memory <- paste(in.memory$partenza, in.memory$arrivo, sep=sep)
  
  da.inserire <- setdiff(in.memory, in.db)
  da.eliminare <- setdiff(in.db, in.memory)

  df <- if(length(hash::keys(data))) {
    ## cerco archi aggiunti di recente.
    DBI::dbGetQuery(con, getSQLbyKey(
      helper, "ARCHI_TAG_LAST_UPDATED",
      tag=tag, last_updated=round(as.numeric(timestamp))))
  } else {
    data.frame(partenza=character(0), arrivo=character(0))
  }
  
  if(nrow(df) > 0) {
    ## controllo che i nuovi archi non siano tra le serie che ho modificato e
    ## che non creino un anello
    wood <- igraph::graph.data.frame(df, directed=TRUE)
    network_aux <- igraph::graph.union(network, wood)
    if(any(hash::keys(functions) %in% df$arrivo)) {
      warning("Ci sono conflitti sugli archi, continuo su dati e formule")    
    }
    checkDAG(network_aux)    
  }
  
  if(length(da.inserire)) {
    params <- if(length(da.inserire) == 1) {
      tokens <- stringr::str_split(da.inserire, sep)[[1]]
      df <- as.data.frame(
        list(
          partenza = tokens[[1]],
          arrivo=tokens[[2]]),
        stringsAsFactors = F)
      names(df) <- c("partenza", "arrivo")
      df
    } else {
      splitted <- unlist(stringr::str_split(da.inserire, sep))
      df <- as.data.frame(matrix(splitted, nrow=length(da.inserire), byrow=T),
                          stringsAsFactors = F)
      names(df) <- c("partenza", "arrivo")
      df
    }
    params <- c(tag, df, autore)
    
    foreach::foreach(row=iterators::iter(df, by='row')) %do% {
      from <- row$partenza
      to <- row$arrivo
      DBI::dbExecute(con, getSQLbyKey(
        helper, "INSERT_ARCHI", tag=tag, from=from, to=to,
        autore=autore, last_updated=time.in.millis()))
    }
  }

  if(length(da.eliminare)) {
    params <- if(length(da.eliminare) == 1) {
      tokens <- str_split(da.eliminare, sep)[[1]]
      df <- as.data.frame(
        list(
          partenza = tokens[[1]],
          arrivo=tokens[[2]]),
        stringsAsFactors = F)
      names(df) <- c("partenza", "arrivo")
      df
    } else {
      splitted <- unlist(stringr::str_split(da.eliminare, sep))
      df <- as.data.frame(matrix(splitted, nrow=length(da.eliminare), byrow=T),
                          stringsAsFactors = F)
      names(df) <- c("partenza", "arrivo")
      df
    }
    params <- c(tag, df, autore)
    
    foreach::foreach(row=iterators::iter(df, by='row')) %do% {
      from <- row$partenza
      to <- row$arrivo
      DBI::dbExecute(con, getSQLbyKey(
        helper, "DELETE_ARCHI", tag=tag, from=from, to=to))
    }
  }
  
  if(interactive()) flog.info("Update Archi done.", name=ln)
}
