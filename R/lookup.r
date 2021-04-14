#' Ricerca nei metadati del grafo
#'
#' @usage lookup_impl(x, key, value)
#' @name lookup_impl
#' @param x istanza di grafo
#' @param key chiave del metadato
#' @param value valore del metadato
#' @return lista di nomi che matchano la condizione `key==valy`
#' @include db.r

lookup_impl <- function(x, key, value) {
  tag <- x@tag
  con <- build_connection()
  on.exit(disconnect(con))
  ## non ci sono prepared statement funzionanti. maledetti.
  helper <- x@helper

  df <- DBI::dbGetQuery(con, sql_by_key(
    helper, "LOOKUP_METADATI",
    tag = tag,
    key = key,
    value = value))

  as.character(df$name)
}

#' Ricerca nelle formule del grafo
#'
#' @usage lookup_formula_impl(x, key)
#' @name lookup_formula_impl
#' @param x istanza di grafo
#' @param key Stringa da cercare
#' @return lista di nomi che hanno `key` nella formula
#' @include db.r

lookup_formula_impl <- function(x, key) {
  tag <- x@tag
  helper <- x@helper
  con <- build_connection()
  on.exit(disconnect(con))
  ## non ci sono prepared statement funzionanti. maledetti.
  df <- DBI::dbGetQuery(con, sql_by_key(
    helper, "LOOKUP_FORMULA",
    tag = tag,
    key = as.character(key)))

  as.character(df$name)
}

#' Ricerca nelle formule del grafo
#'
#' @usage lookup_dati_impl(x, key)
#' @name lookup_dati_impl
#' @param x istanza di grafo
#' @param key numero da cercare
#' @return lista di nomi che matchano `key` nella formula
#' @include db.r

lookup_dati_impl <- function(x, key) {
  tag <- x@tag
  helper <- x@helper
  con <- build_connection()
  on.exit(disconnect(con))
  ## non ci sono prepared statement funzionanti. maledetti.
  df <- DBI::dbGetQuery(con, sql_by_key(
    helper, "LOOKUP_DATI",
    tag = tag,
    key = as.character(key)))

  as.character(df$name)
}
