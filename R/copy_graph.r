#' @include sqlhelper.r
#' @include persistence_utils.r

copy_graph <- function(from, to, con, ...) {
  param_list <- list(...)
  commento <- rutils::ifelse("msg" %in% names(param_list),
    param_list[["msg"]],
    paste0("Rilascio per ", to))

  helper <- rutils::ifelse("helper" %in% names(param_list),
    param_list[["helper"]],
    SQLHelper())

  last_updated <- rutils::ifelse("last_updated" %in% names(param_list),
    param_list[["last_updated"]],
    time_in_nano())

  autore <- rutils::ifelse("autore" %in% names(param_list),
    param_list[["autore"]],
    rutils::whoami())

  ## copia archi
  DBI::dbExecute(con, sql_by_key(
    helper, "COPY_DATI", to = to, from = from))

  ## copia archi
  DBI::dbExecute(con, sql_by_key(
    helper, "COPY_ARCHI", to = to, from = from))

  ## copia formule
  DBI::dbExecute(con, sql_by_key(
    helper, "COPY_FORMULE", to = to, from = from))

  ## copio metadati
  DBI::dbExecute(con, sql_by_key(
    helper, "COPY_METADATI", to = to, from = from))

  DBI::dbExecute(con, sql_by_key(
    helper, "INSERT_GRAFI", tag = to,
    commento = commento, autore = autore,
    last_updated = time_in_nano()))
}
