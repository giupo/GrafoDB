#' @include sqlhelper.r
#' @include persistence_utils.r

copy_graph <- function(from, to, con, ...) {
  param_list <- list(...)
  commento <- if('msg' %in% names(param_list)) {
    param_list[["msg"]]
  } else {
    paste0("Rilascio per ", to)
  }

  helper <- if("helper" %in% names(param_list)) {
    param_list[["helper"]]
  } else {
    SQLHelper()
  }

  last_updated <- if("last_updated" %in% names(param_list)) {
    param_list[["last_updated"]]
  } else {
    time.in.millis()
  }

  autore <- if('autore' %in% names(param_list)) {
    param_list[["autore"]]
  } else {
    rutils::whoami()
  }

  ## copia archi
  DBI::dbExecute(con, getSQLbyKey(
    helper, "COPY_DATI", to = to, from = from))

  ## copia archi
  DBI::dbExecute(con, getSQLbyKey(
    helper, "COPY_ARCHI", to = to, from = from))

  ## copia formule
  DBI::dbExecute(con, getSQLbyKey(
    helper, "COPY_FORMULE", to = to, from = from))

  ## copio metadati
  DBI::dbExecute(con, getSQLbyKey(
    helper, "COPY_METADATI", to = to, from = from))

  DBI::dbExecute(con, getSQLbyKey(
    helper, "INSERT_GRAFI", tag = to,
    commento = commento, autore = autore,
    last_updated=time.in.millis()))
}
