#' @importFrom DBI dbGetQuery
#' @importFrom RPostgreSQL dbGetQuery
#' @include db.r

.getMeta <- function(x, serie, metadato) {
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  helper <- x@helper
  tag <- x@tag
  df <- dbGetQuery(con, getSQLbyKey(
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
#' @importFrom DBI dbGetQuery
#' @importFrom RPostgreSQL dbGetQuery
#' @include db.r

.getMetadata <- function(x, name) {
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  tag <- x@tag
  helper <- x@helper
  sql <-  getSQLbyKey(helper, "GET_METADATA", tag=tag, name=name)
  dbGetQuery(con, sql)
}

#' ritorna le chiavi di un metadato
#'
#' @name .keys
#' @usage .keys(x)
#' @param x istanza di grafo
#' @return ritorna un dataframe con le chiavi dei metadati
#' @importFrom hash keys
#' @rdname keys-internal
#' @include db.r
#' @importFrom DBI dbGetQuery
#' @importFrom RPostgreSQL dbGetQuery

.keys <- function(x) {
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  
  tag <- x@tag
  helper <- x@helper
  sql <- getSQLbyKey(helper, "KEYS_METADATA", tag=tag)
  dbGetQuery(con, sql)
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
#' @importFrom DBI dbGetQuery
#' @importFrom RPostgreSQL dbGetQuery

.values <- function(x, key=NULL) {
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  tag <- x@tag
  helper <- x@helper
  sql <- if(is.null(key)) {
    getSQLbyKey(helper, "VALUES_METADATA", tag=tag)
  } else {
    getSQLbyKey(helper, "VALUES_METADATA_KEY", tag=tag, key=key)
  }
  df <- dbGetQuery(con, sql)
  as.character(df[,1])
}


#' elimina un metadato dal DBI
#'
#' @name .deleteMeta
#' @usage .deleteMeta(x, name, key, value)
#' @param x istanza di grafo
#' @param name nome della serie da cui eliminare il metadato
#' @param key nome del metadato
#' @param value valore del metadato
#' @rdname deleteMeta-internal
#' @include db.r
#' @importFrom DBI dbGetQuery
#' @importFrom RPostgreSQL dbGetQuery

.deleteMeta <- function(x, name, key, value) {
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  tag <- x@tag
  helper <- x@helper
  
  dbGetQuery(con, getSQLbyKey(
    helper, "DELETE_META_TAG_NAME_KEY_VALUE",
    tag=tag,
    name=name,
    key=key,
    value=value))

  invisible(NULL)
}

#' @importFrom rutils whoami
#' @include db.r sqlhelper.r
#' @importFrom DBI dbGetQuery
#' @importFrom RPostgreSQL dbGetQuery

.setMeta <- function(x, name, key, value) {
  nomiobj <- names(x)
  if(!all(name %in% nomiobj)) {
    nong <- setdiff(name, nomiobj)
    stop("Non e' una serie del grafo: ", paste(nong, collapse=", "))
  }
  
  domain <- lookup(x, key, value)
  if(any(name %in% domain)) {
    already <- intersect(domain, name)
    warning("Ha gia' un metadato ", key, " = ", value, " :", paste(already, collapse=", "))
  } else {
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    tag <- x@tag
    helper <- x@helper
    autore <- whoami()
    
    sql <- getSQLbyKey(
      helper, "INSERT_META",
      tag=tag,
      name=name,
      key=key,
      value=value,
      autore=autore)
    
    dbGetQuery(con, sql)
  }
  x
}
