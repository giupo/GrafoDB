
#' @include logging.r
#' @include assert_dag.r

update_edges <- function(x, con, tag=x@tag) {
  ln <- "GrafoDB::update_edges"

  if (interactive()) info("Update Edges ...", name = ln)

  data <- x@data
  functions <- x@functions
  timestamp <- x@timestamp
  helper <- x@helper
  network <- x@network
  in.memory <- as.data.frame(igraph::get.edgelist(network),
                             stringsAsFactors = F)
  autore <- rutils::whoami()
  names(in.memory) <- c("partenza", "arrivo")
  in.db <- DBI::dbGetQuery(con, sql_by_key(helper, "ARCHI_TAG", tag = tag))

  sep <- "-"
  in.db <- gdata::drop.levels(in.db)
  in.db <- paste(in.db$partenza, in.db$arrivo, sep=sep)
  in.memory <- paste(in.memory$partenza, in.memory$arrivo, sep=sep)

  da.inserire <- setdiff(in.memory, in.db)
  da.eliminare <- setdiff(in.db, in.memory)

  df <- if (length(hash::keys(data))) {
    ## cerco archi aggiunti di recente.
    DBI::dbGetQuery(con, sql_by_key(
      helper, "ARCHI_TAG_LAST_UPDATED",
      tag = tag, last_updated=round(as.numeric(timestamp))))
  } else {
    data.frame(partenza=character(0), arrivo=character(0))
  }

  if (nrow(df) > 0) {
    ## controllo che i nuovi archi non siano tra le serie che ho modificato e
    ## che non creino un anello
    wood <- igraph::graph.data.frame(df, directed=TRUE)
    network_aux <- igraph::graph.union(network, wood)
    if (any(hash::keys(functions) %in% df$arrivo)) {
      warning("There are conflicts on edges, keep working on data and formula")
    }
    assert_dag(network_aux)
  }

  if (length(da.inserire)) {
    params <- if (length(da.inserire) == 1) {
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
      df <- as.data.frame(
        matrix(splitted, nrow=length(da.inserire), byrow=T),
        stringsAsFactors = F)
      names(df) <- c("partenza", "arrivo")
      df
    }
    params <- c(tag, df, autore)

    foreach::`%do%`(foreach::foreach(row=iterators::iter(df, by="row")), {
      from <- row$partenza
      to <- row$arrivo
      DBI::dbExecute(con, sql_by_key(
        helper, "INSERT_ARCHI", tag = tag, from=from, to=to,
        autore=autore, last_updated=time_in_nano()))
    })
  }

  if (length(da.eliminare)) {
    params <- if (length(da.eliminare) == 1) {
      tokens <- stringr::str_split(da.eliminare, sep)[[1]]
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

    foreach::`%do%`(foreach::foreach(row=iterators::iter(df, by="row")), {
      from <- row$partenza
      to <- row$arrivo
      DBI::dbExecute(con, sql_by_key(
        helper, "DELETE_ARCHI", tag = tag, from=from, to=to))
    })
  }

  if (interactive()) info("Update Edges done.", name = ln)
}
