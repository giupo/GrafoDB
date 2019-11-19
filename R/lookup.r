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

.lookup <- function(x, key, value) {
  tag <- x@tag
  con <- buildConnection()
  on.exit(disconnect(con))
  ## non ci sono prepared statement funzionanti. maledetti.
  helper <- x@helper

  df <- dbGetQuery(con, getSQLbyKey(
    helper, "LOOKUP_METADATI",
    tag=tag,
    key=key,
    value=value))

  as.character(df$name)
}

#' Ricerca nelle formule del grafo
#'
#' @usage .lookup_formula(x, key)
#' @name .lookup_formula
#' @rdname lookup-formula-internal
#' @param x istanza di grafo
#' @param key Stringa da cercare
#' @return lista di nomi che hanno `key` nella formula
#' @include db.r
#' @importFrom DBI dbGetQuery

.lookup_formula <- function(x, key) {
  tag <- x@tag
  helper <- x@helper
  con <- buildConnection()
  on.exit(disconnect(con))
  ## non ci sono prepared statement funzionanti. maledetti.
  df <- dbGetQuery(con, getSQLbyKey(
    helper, "LOOKUP_FORMULA",
    tag=tag,
    key=as.character(key)))

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

.lookup_dati <- function(x, key) {
  tag <- x@tag
  helper <- x@helper
  con <- buildConnection()
  on.exit(disconnect(con))
  ## non ci sono prepared statement funzionanti. maledetti.
  df <- dbGetQuery(con, getSQLbyKey(
    helper, "LOOKUP_DATI",
    tag=tag,
    key=as.character(key)))
  
  as.character(df$name)
}
