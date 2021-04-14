#' Legge tutta la tabella dei dati per un determinato tag
#'
#' @name load_table
#' @usage load_table(table_name, tag)
#' @usage load_table(table_name, tag, con)
#' @return un data.frame con i dati delle serie storiche
#' @param table_name nome della tabella
#' @param tag nome del tag
#' @note funzione interna
#' @include db.r

load_table <- function(table_name, tag, con = NULL) {
  con <- if (is.null(con)) {
    cc <- build_connection()
    on.exit(disconnect(cc))
    cc
  } else {
    con
  }

  full_table_name <- rutils::ifelse(
    class(con) != "SQLiteConnection",
    paste0(table_name, '_', tag),
    table_name)

  df <- rutils::ifelse(
    DBI::dbExistsTable(con, full_table_name),
    DBI::dbReadTable(con, full_table_name),
    stop(full_table_name, " doesn't exist"))


  # filter out in case of RSQLite
  df <- df[df$tag == tag, ]

  unique(df)
}

load_data <- function(tag, con=NULL) tryCatch({
  load_table("dati", tag, con = con)
}, error = function(cond) {
  data.frame(
    name = character(),
    year = integer(),
    period = integer(),
    freq = integer(),
    dati = character(),
    stato = integer(),
    notes = character(),
    autore = character())
})

load_edges <- function(tag, con=NULL) tryCatch({
  load_table("archi", tag, con = con)
}, error = function(cond) {
  data.frame(partenza = character(), arrivo = character())
})

load_metadata <- function(tag, con=NULL) load_table("metadati", tag, con = con)

load_formulas <- function(tag, con=NULL) tryCatch({
  load_table("formule", tag, con = con)
}, error=function(cond) {
  data.frame(
    name = character(),
    tag = character(),
    formula = character(),
    autore = character())
})

load_grafi <- function(con=NULL) {
  con <- if (is.null(con)) {
    con <- build_connection()
    on.exit(disconnect(con))
    con
  } else {
    con
  }
  DBI::dbReadTable(con, "grafi")
}


create_new_grafo <- function(x, tag, con = NULL,
  msg = paste0("Grafo per ", tag)) {
  autore <- rutils::whoami()
  # FIXME: Devo usare i timestamp di R o del DBMS?
  x@timestamp <- time_in_nano()
  helper <- x@helper
  sql <- sql_by_key(
    helper, "CREATE_NEW_GRAFO",
    tag = tag,
    commento = msg,
    autore = autore,
    last_updated = x@timestamp)


  con <- if (is.null(con)) {
    con <- build_connection()
    on.exit({
      disconnect(con)
    })
    con
  } else {
    con
  }

  tryCatch({
    DBI::dbBegin(con)
    DBI::dbExecute(con, sql)
    DBI::dbCommit(con)
  }, error = function(cond) {
    tryCatch(DBI::dbRollback(con), error = function(cx) {
      stop(cx, ", Root: ", cond)
    })
    stop(cond)
  })
  x
}


resync <- function(x, con = NULL) {
  if (is.null(con)) {
    con <- build_connection()
    on.exit(disconnect(con))
    con
  } else {
    con
  }

  tag <- x@tag
  x@dbdati <- load_data(tag, con = con)
  x@dbformule <- load_formulas(tag, con = con)
  ## e gli archi :?? :)
  x
}


need_resync <- function(x) {
  timestamp <- x@timestamp
  helper <- x@helper
  con <- build_connection()
  on.exit(disconnect(con))
  tag <- x@tag
  df <- DBI::dbGetQuery(con, sql_by_key(
    helper, "NEED_RESYNC", tag = tag,
    last_updated = as.character(timestamp)))

  df[[1]] > 0
}

time_in_nano <- function() as.numeric(Sys.time()) * 1000
