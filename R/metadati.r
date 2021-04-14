#' @include db.r

.getMeta <- function(x, serie, metadato) {
  con <- build_connection()
  on.exit(disconnect(con))
  helper <- x@helper
  tag <- x@tag
  df <- DBI::dbGetQuery(con, sql_by_key(
    helper, "GET_META", tag=tag, name=serie, key=metadato))
  if(nrow(df)) {
    as.character(df[,1])
  } else {
    character()
  }
}


#' Ritorna i metadati della serie `name` in `x`
#'
#' @name .getMetadata
#' @rdname getMetadata-internal
#' @usage .getMetadata(x, name)
#' @param x istanza di grafo
#' @param name nome della serie storica
#' @return data.frame contenente i metadati della serie
#' @include db.r

.getMetadata <- function(x, name) {
  con <- build_connection()
  on.exit(disconnect(con))
  tag <- x@tag
  helper <- x@helper
  sql <-  sql_by_key(helper, "GET_METADATA", tag = tag, name = name)
  DBI::dbGetQuery(con, sql)
}

#' Ritorna le chiavi dei metadati
#'
#' @name .keys
#' @usage .keys(x)
#' @param x istanza di grafo
#' @return ritorna un dataframe con le chiavi dei metadati
#' @rdname keys-internal
#' @include db.r

.keys <- function(x) {
  con <- build_connection()
  on.exit(disconnect(con))
  tag <- x@tag
  helper <- x@helper
  sql <- sql_by_key(helper, "KEYS_METADATA", tag=tag)
  DBI::dbGetQuery(con, sql)
}


#' Ritorna i valori dei metadati (o per singolo metadato)
#'
#' @name .values
#' @usage .values(x)
#' @usage .values(x, key)
#' @rdname values-internal
#' @param x istanza di grafo
#' @param key chiave del metadato
#' @return lista di valori per metadato
#' @include db.r

.values_by_key <- function(x, key=NULL) {
  con <- build_connection()
  on.exit(disconnect(con))
  tag <- x@tag
  helper <- x@helper
  sql <- if(is.null(key)) {
    sql_by_key(helper, "VALUES_METADATA", tag=tag)
  } else {
    sql_by_key(helper, "VALUES_METADATA_KEY", tag=tag, key=key)
  }

  df <- DBI::dbGetQuery(con, sql)
  as.character(df[,1])
}


#' Ritorna i valore del lmetadato per ogni singola serie
#'
#' @name .values
#' @usage .values(x, name, key)
#' @rdname values-internal
#' @param x istanza di grafo
#' @param key chiave del metadato
#' @return lista di valori per metadato
#' @include db.r
#' @export

values_for <- function(x, name=names(x), key=keys(x)) {
  if (is.null(name)) {
    stop("name cannot be null")
  }
  if (is.null(key)) {
    stop("key cannot be null")
  }
  con <- build_connection()
  on.exit(disconnect(con))
  tag <- x@tag
  helper <- x@helper
  sql <- sql_by_key(helper, "VALUES_BY_NAME_AND_KEY")
  sql <- whisker::whisker.render(sql, list(
    nomi=paste(shQuote(name), collapse=", "),
    chiavi=paste(shQuote(key), collapse=", "),
    tag=tag
  ))

  DBI::dbGetQuery(con, sql)
}

#' delete un metadato dal DBI
#'
#' @name .deleteMeta
#' @usage .deleteMeta(x, name, key, value)
#' @param x istanza di grafo
#' @param name nome della serie da cui eliminare il metadato
#' @param key nome del metadato
#' @param value valore del metadato (se non specificato, rimuove tutti i  metadati
#'              con la chiave specificata)
#' @rdname deleteMeta-internal
#' @include db.r

.deleteMeta <- function(x, name, key, value=NULL) {
  if(is.null(value)) {
    return(.deleteMetaByKey(x, name, key))
  }
  con <- build_connection()
  on.exit(disconnect(con))
  tag <- x@tag
  helper <- x@helper

  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(con, sql_by_key(
      helper, "DELETE_META_TAG_NAME_KEY_VALUE",
      tag=tag,
      name=name,
      key=key,
      value=value))
    DBI::dbCommit(con)
  }, error = function(cond) {
    DBI::dbRollback(con)
    stop(cond)
  })
  
  invisible(x)
}

.deleteMetaByKey <- function(x, name, key) {
    con <- build_connection()
    on.exit(disconnect(con))

    tag <- x@tag
    helper <- x@helper

    DBI::dbBegin(con)
    tryCatch({
      DBI::dbExecute(con, sql_by_key(
        helper, "DELETE_META_TAG_NAME_KEY",
        tag=tag,
        name=name,
        key=key))
      DBI::dbCommit(con)
    }, error = function(cond) {
      DBI::dbRollback(con)
      stop(cond)
    })
}

#' @include db.r sqlhelper.r

.setMeta <- function(x, name, key, value) {
  nomiobj <- names(x)
  if(!all(name %in% nomiobj)) {
    nong <- setdiff(name, nomiobj)
    stop("Non e' una serie del grafo: ", paste(nong, collapse=", "))
  }
  con <- build_connection()
  on.exit(disconnect(con))

  helper <- x@helper

  df <- DBI::dbGetQuery(con, sql_by_key(
    helper, "LOOKUP_METADATI",
    tag=x@tag,
    key=key,
    value=value))

  domain <- as.character(df$name)

  if(any(name %in% domain)) {
    already <- intersect(domain, name)
    warning("Ha gia' un metadato ", key, " = ", value, " :", paste(already, collapse=", "))
  } else {
    tag <- x@tag
    helper <- x@helper
    autore <- rutils::whoami()

    sql <- sql_by_key(
      helper, "INSERT_META",
      tag=tag,
      name=name,
      key=key,
      value=value,
      autore=autore)

    DBI::dbExecute(con, sql)
  }
  x
}
