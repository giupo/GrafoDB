#' ricerca nei metadati del `GrafoDB`
#'
#' @name lookup
#' @usage lookup(x, key, value)
#' @param x istanza di `GrafoDB`
#' @param key `character` che specifica la chiave del metadato
#' @param value `character` che specifica il valore del metadato
#' @return un character array di nomi di serie che rispettano la clausola `key` = `value`. Se non esistono ritorna un character(0) (array vuoto)
#' @examples \dontrun{
#' g = GrafoDB(...) # istanzia il grafo
#' lookup(g, "TAVOLA_DI_OUTPUT", "BRI") # ritorna i nomi di serie che hanno TAVOLA_DI_OUTPUT=BRI
#' }
#' @rdname lookup_generic

setGeneric(
  "lookup",
  function(x, key, value) {
    standardGeneric("lookup")
  })

#' Formule del `GrafoDB`
#'
#' Ritorna come named list le formule per ogni serie specificata in `nomi`
#'
#' @name expr
#' @usage expr(x, nomi)
#' @param x istanza di oggetto R
#' @param nomi character array di nomi di serie storiche
#' @param echo stampa con un messaggio su standard output il valore della formula
#' @return `list` con nomi (i nomi sono gli stess del parametro `nomi`) con le formule
#' @examples \dontrun{
#' g <- GrafoDB(...)
#' expr(g, "TETSZ0AC") # ritorna list(TETSZ0AC = "TETSZ0AC = ASTSZ0AC...")
#' }
#' @rdname expr_generic

setGeneric(
  "expr",
  function(x, nomi, echo=TRUE) {
    standardGeneric("expr")
  })

#' Ritorna `TRUE` se il grafo ha conflitti
#'
#' @name hasConflicts
#' @usage hasConflicts(x)
#' @usage hasConflicts(x, name)
#' @param x oggetto R
#' @param name character array di nomi (puo' essere omesso)
#' @return `TRUE` se l'istanza `x` e' un GrafoDB con conflitti, `FALSE` altrimenti
#' @examples \dontrun{
#' g <- GrafoDB(...)
#' hasConflicts(g) # dovrebbe essere FALSE
#' ...             # eseguo operazioni come modificare la stessa serie
#' hasConflicts(g) # TRUE
#' hasConflicts(g, SERIE_CON_CONFLITTO) # TRUE
#' hasConflicts(g, SERIE_SENZA_CONFLITTO) # FALSE
#' }

setGeneric(
  "hasConflicts",
  function(x, name=NULL) {
    standardGeneric("hasConflicts")
  })

setGeneric(
  "getConflicts",
  function(x, name=NULL) {
    standardGeneric("getConflicts")
  })

setGeneric(
  "isRoot",
  function(x, name) {
    standardGeneric("isRoot")
  })

setGeneric(
  "upgrf",
  function(x, name, livello=.Machine$integer.max) {
    standardGeneric("upgrf")
  })

setGeneric(
  "downgrf",
  function(x, name, livello=.Machine$integer.max) {
    standardGeneric("downgrf")
  })
