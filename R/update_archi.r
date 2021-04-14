
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
  in_memory <- as.data.frame(igraph::get.edgelist(network),
                             stringsAsFactors = FALSE)
  autore <- rutils::whoami()
  names(in_memory) <- c("partenza", "arrivo")
  in_db <- DBI::dbGetQuery(con, sql_by_key(helper, "ARCHI_TAG", tag = tag))

  sep <- "-"
  in_db <- gdata::drop.levels(in_db)
  in_db <- paste(in_db$partenza, in_db$arrivo, sep = sep)
  in_memory <- paste(in_memory$partenza, in_memory$arrivo, sep = sep)

  to_be_inserted <- setdiff(in_memory, in_db)
  to_be_deleted <- setdiff(in_db, in_memory)

  df <- if (length(hash::keys(data))) {
    ## cerco archi aggiunti di recente.
    DBI::dbGetQuery(con, sql_by_key(
      helper, "ARCHI_TAG_LAST_UPDATED",
      tag = tag, last_updated = round(as.numeric(timestamp))))
  } else {
    data.frame(partenza = character(0), arrivo = character(0))
  }

  if (nrow(df) > 0) {
    ## controllo che i nuovi archi non siano tra le serie che ho modificato e
    ## che non creino un anello
    wood <- igraph::graph.data.frame(df, directed = TRUE)
    network_aux <- igraph::graph.union(network, wood)
    if (any(hash::keys(functions) %in% df$arrivo)) {
      warning("There are conflicts on edges, keep working on data and formula")
    }
    assert_dag(network_aux)
  }

  if (length(to_be_inserted)) {
    params <- if (length(to_be_inserted) == 1) {
      tokens <- stringr::str_split(to_be_inserted, sep)[[1]]
      df <- as.data.frame(
        list(
          partenza = tokens[[1]],
          arrivo = tokens[[2]]),
        stringsAsFactors = FALSE)
      names(df) <- c("partenza", "arrivo")
      df
    } else {
      splitted <- unlist(stringr::str_split(to_be_inserted, sep))
      df <- as.data.frame(
        matrix(splitted, nrow = length(to_be_inserted), byrow = TRUE),
        stringsAsFactors = FALSE)
      names(df) <- c("partenza", "arrivo")
      df
    }
    params <- c(tag, df, autore)

    foreach::`%do%`(foreach::foreach(row = iterators::iter(df, by = "row")), {
      from <- row$partenza
      to <- row$arrivo
      DBI::dbExecute(con, sql_by_key(
        helper, "INSERT_ARCHI",
        tag = tag,
        from = from,
        to = to,
        autore = autore,
        last_updated = time_in_nano()))
    })
  }

  if (length(to_be_deleted)) {
    params <- if (length(to_be_deleted) == 1) {
      tokens <- stringr::str_split(to_be_deleted, sep)[[1]]
      df <- as.data.frame(
        list(
          partenza = tokens[[1]],
          arrivo = tokens[[2]]),
        stringsAsFactors = FALSE)
      names(df) <- c("partenza", "arrivo")
      df
    } else {
      splitted <- unlist(stringr::str_split(to_be_deleted, sep))

      df <- as.data.frame(
        matrix(splitted,
          nrow = length(to_be_deleted), byrow = TRUE),
        stringsAsFactors = FALSE)

      names(df) <- c("partenza", "arrivo")
      df
    }
    params <- c(tag, df, autore)

    foreach::`%do%`(foreach::foreach(row = iterators::iter(df, by = "row")), {
      from <- row$partenza
      to <- row$arrivo
      DBI::dbExecute(con, sql_by_key(
        helper, "DELETE_ARCHI",
        tag = tag,
        from = from,
        to = to))
    })
  }

  if (interactive()) info("Update Edges done.", name = ln)
}
