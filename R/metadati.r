#' @importFrom DBI dbGetQuery
#' @importFrom RPostgreSQL dbGetQuery
#' @include db.r

.getMeta <- function(x, serie, metadato) {
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  df <- dbGetQuery(
    con,
    paste0("select value from metadati where tag='", x@tag,
           "' and name='", serie, "' and key='", metadato,"' order by 1"))
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
  sql <-  paste0("select name, key, value from metadati where tag = '",x@tag,
                 "' and name = '", name, "' order by name, key")
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
  sql <- paste0("select distinct key from metadati where tag=", x@tag," order by 1")
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
  df <- if(is.null(key)) {
    sql <- paste0("select distinct value from metadati where tag='", tag,"' order by 1")
    dbGetQuery(con, sql)
  } else {
    sql <- paste0("select distinct value from metadati where tag='", tag,
                  "' and key='", key, "' order by 1")
    dbGetdQuery(con, sql)
  }
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
  tag <- object@tag
  params <- data.frame(tag=tag, name=tsName, key=attrName, value=attrValue)
  sql <- paste0("delete from metadati where tag = '", tag,
                "' and name= '", name, "' and key = '", key, "' and value = '", value, "'")
  dbGetQuery(con, sql)
  invisible(NULL)
}

#' @importFrom rutils whoami
#' @include db.r
#' @importFrom DBI dbGetQuery
#' @importFrom RPostgreSQL dbGetQuery

.setMeta <- function(x, name, key, value) {
  nomiobj <- names(x)
  if(!all(tsName %in% nomiobj)) {
    nong <- setdiff(name, nomiobj)
    stop("Non e' una serie del grafo: ", paste(nong, collapse=", "))
  }
  
  domain <- lookup(x, key, value)
  if(any(name %in% domain)) {
    already <- intersect(domain, tsName)
    warning("Ha gia' un metadato ", key, " = ", value, " :", paste(already, collapse=", "))
  } else {
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    sql <- paste0("insert into metadati(tag, name, key, value, autore) values ('", x@tag,"', '",
                  name, "', '", key, "', '", value , "', '", whoami(), "')")
    
    dbGetQuery(con, sql)
  }
  object
}
