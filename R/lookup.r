#' Ricerca nei metadati del grafo
#'
#' @usage .lookup(x, key, value)
#' @name .lookup
#' @rdname lookup-internal
#' @param x istanza di grafo
#' @param key chiave del metadato
#' @param value valore del metadato
#' @return lista di nomi che matchano la condizione `key==valy`
#' @include db.r
#' @importFrom DBI dbGetQuery
#' @importFrom RPostgreSQL dbGetQuery

.lookup <- function(x, key, value) {
  tag <- x@tag
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  ## non ci sono prepared statement funzionanti. maledetti.

  sql <- paste0("select name from dati where tag = '", tag,
                "' and key = '%",key,"%' and value = '", value,"'")
  
  df <- dbGetQuery(con, sql)
  as.character(df$name)
}

#' Ricerca nelle formule del grafo
#'
#' @usage .lookup_formula(x, key)
#' @name .lookup_formula
#' @rdname lookup-formula-internal
#' @param x istanza di grafo
#' @param key Stringa da cercare
#' @return lista di nomi che matchano `key` nella formula
#' @include db.r
#' @importFrom DBI dbGetQuery
#' @importFrom RPostgreSQL dbGetQuery

.lookup_formula <- function(x, key) {
  tag <- x@tag
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  ## non ci sono prepared statement funzionanti. maledetti.
  sql <- paste0("select name from formule where tag = '", tag,"' and formula like '%",key,"%'")
  df <- dbGetQuery(con, sql)
  as.character(df$name)
}

#' Ricerca nelle formule del grafo
#'
#' @usage .lookup_dati(x, key)
#' @name .lookup_dati
#' @rdname lookup-dati-internal
#' @param x istanza di grafo
#' @param key numero da cercare
#' @return lista di nomi che matchano `key` nella formula
#' @include db.r
#' @importFrom DBI dbGetQuery
#' @importFrom RPostgreSQL dbGetQuery

.lookup_dati <- function(x, key) {
  tag <- x@tag
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  ## non ci sono prepared statement funzionanti. maledetti.
  sql <- paste0("select name from formule where tag = '", tag,
                "' and dati like '%",as.character(key),"%'")
  df <- dbGetQuery(con, sql)
  as.character(df$name)
}
