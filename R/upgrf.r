#' Ritorna i genitori delle serie
#'
#' ritorna i nomi delle serie che entrano nelle serie date in `name`
#'
#' @name upgrf
#' @param x un istanza di GrafoDB
#' @param name array di nomi di serie
#' @param livello numero di livelli (ordine) da considerare (di default, tutti)
#' @return nomi di serie
#' @export
#' @exportMethod upgrf
#' @include navigate.r

methods::setGeneric(
  "upgrf",
  function(x, name, livello=.Machine$integer.max) {
    standardGeneric("upgrf")
  })

#' @rdname upgrf

methods::setMethod(
  "upgrf",
  signature("GrafoDB", "character", "ANY"),
  function(x, name, livello=.Machine$integer.max) {
    navigate(x, name, order = livello, mode = "in")
  })