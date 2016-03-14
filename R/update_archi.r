#' @importFrom gdata drop.levels
#' @importFrom igraph get.edgelist graph.union graph.data.frame is.dag topological.sort
#' @importFrom stringr str_split
#' @importFrom RPostgreSQL2 dbGetpreparedQuery
#' @importFrom rutils whoami

.updateArchi <- function(x, con, tag=x@tag) {
  if(interactive()) cat("Update Archi...")

  data <- x@data
  functions <- x@functions
  timestamp <- x@timestamp
  network <- x@network
  in.memory <- as.data.frame(get.edgelist(network), stringsAsFactors = F)
  autore <- whoami()
  names(in.memory) <- c("partenza", "arrivo")
  in.db <- dbGetPreparedQuery(
    con,
    paste("select partenza, arrivo from archi where tag = ?"),
    bind.data = tag)
  sep <- "-"
  in.db <- drop.levels(in.db)
  in.db <- paste(in.db$partenza, in.db$arrivo, sep=sep)
  in.memory <- paste(in.memory$partenza, in.memory$arrivo, sep=sep)
  
  da.inserire <- setdiff(in.memory, in.db)

  df <- if(length(keys(data))) {
    ## cerco archi aggiunti di recente.
    params <- cbind(tag, timestamp)
    sql <- paste("select partenza, arrivo from archi where tag = ? ",
                 "and last_updated::timestamp(0) > to_timestamp(?)")
    dbGetPreparedQuery(con, sql, bind.data = params)
  } else {
    data.frame(partenza=character(0), arrivo=character(0))
  }
  
  if(nrow(df) > 0) {
    ## controllo che i nuovi archi non siano tra le serie che ho modificato e
    ## che non creino un anello
    wood <- graph.data.frame(df, directed=TRUE)
    network_aux <- graph.union(network, wood)
    if(any(keys(functions) %in% df$arrivo)) {
      warning("Ci sono conflitti sugli archi, continuo su dati e formule")    
    }
    
    if(!is.dag(network_aux)) {
      wrongsort <- try(topological.sort(network), silent=TRUE)
      network_seq <- V(network)
      cycles_seq <- network_seq[setdiff(
        network_seq, network_seq[wrongsort])]
      cycles_vertex <- cycles_seq$name
      stop("Cycles found: ", paste(unlist(cycles_vertex), collapse=", "))
    }
  }
  
  if(length(da.inserire)) {
    params <- if(length(da.inserire) == 1) {
      tokens <- str_split(da.inserire, sep)[[1]]
      df <- as.data.frame(
        list(
          partenza = tokens[[1]],
          arrivo=tokens[[2]]),
        stringsAsFactors = F)
      names(df) <- c("partenza", "arrivo")
      df
    } else {
      splitted <- unlist(str_split(da.inserire, sep))
      df <- as.data.frame(matrix(splitted, nrow=length(da.inserire), byrow=T),
                          stringsAsFactors = F)
      names(df) <- c("partenza", "arrivo")
      df
    }
    params <- c(tag, df, autore)
    dbGetPreparedQuery(
      con,
      "insert into archi(tag, partenza, arrivo, autore) values(?,?,?,?)",
      bind.data = params)
  }
  if(interactive()) cat("Done.\n")
}
