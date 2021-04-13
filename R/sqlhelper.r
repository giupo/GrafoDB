#' Questa classe provvede a disaccoppiare SQL dal codice dell'app
#' 
#' Separa le query SQL con delle chiavi, riponendo il linguaggio SQL in dei file
#' editabili e accedendo alle singole query con delle chiavi.
#'
#' Le query vengono inizializzate in un file .ini
#'
#' @name SQLHelper
#' @usage SQLHelper(path)
#' @aliases SQLHelper
#' @param path path al file contenente le query (default a file interno al package GrafoDB)
#' @param type tipo di database supportati (per ora "PostgreSQL" e "SQLite").
#'             Quest'opzione e' sovrascritta in caso esistano delle options di
#'             ambiente chiamate "SQLHelperType"
#' @title SQLHelper
#' @export SQLHelper
#' @exportClass SQLHelper
#' @include logging.r

SQLHelper <- setClass(
  "SQLHelper",
  methods::representation(
    sqlContainer="list",
    type="character"
  ))



methods::setMethod(
  "initialize",
  signature("SQLHelper"),
  function(.Object, path=NULL, type=NULL) {
    .initSQLHelper(.Object, path=path, type=type)
  })

.initSQLHelper <- function(.Object, path = NULL, type=NULL) {
  env <- getenv()
  if (is.null(type)) {
    type <- if(env == "test") {
      "SQLite"
    } else {
      "PostgreSQL"
    }
  }

  .Object@type <- type
  trace("SQLHeleperType: %s", type, name="GrafoDB.sqlhelper" )
  .Object@sqlContainer <- rutils::ini_parse(
    file.path(system.file(package="GrafoDB", mustWork=T),
              "ini/sql.ini"))[[.Object@type]]

  .Object
}


.sql_by_key <- function(x, .key, ...) {
  sqlContainer <- x@sqlContainer
  if(! .key %in% names(sqlContainer)) {
    stop(.key, " not in query repository")
  }

  ## retrieve sql
  sql <- sqlContainer[[.key]]

  ## handle param list
  params <- list(...)

  for(name in names(params)) {
    paramKey <- paste0("--", name, "--")
    value <- params[[name]]
    if(is.null(value)) {
      value <- ""
    }
    quotedValue <- gsub("'", "''", value)
    sql <- gsub(paramKey, quotedValue, sql)
  }

  ## check if any params is left behind

  idx <- stringr::str_locate_all(sql, "--[A-Z|a-z|0-9]*--")[[1]]
  if (length(idx) > 0) {
    for(irow in 1:nrow(idx)) {
      start <- idx[irow, 1]
      end <- idx[irow, 2]
      param <- substring(sql, first=start, last=end)
      warning(param, " has not been set")
    }
    stop("params for query ", .key, " of type ", x@type, " have not been set")
  }


  trace("Query for key '%s' = %s", .key, sql, name = 'GrafoDB.sqlhelper')
  sql
}

#' Factory di query (stringhe) SQL.
#'
#' Ritorna le query SQL cosi' come storicizzate nell'SQLHelper. Sostituisce
#' eventuali parametri (key=value) della query nell'ellipsis del metodo
#'
#' Le query hanno una forma "select * from --tab-- where param='--value--'";
#' Il metodo si occupa di sostiture '--tab--' e '--value--' secondo i valori
#' passati come argomento: 
#' Es: `sql_by_key(helper, KEYQUERY, tab="A", value="B")`
#' risultera' in `select * from A where param='B'`
#' 
#' @name sql_by_key
#' @usage sql_by_key(x, key)
#' @usage sql_by_key(x, key, param=value, param2, value2, ...)
#' 
#' @param x istanza di SQLHelper
#' @param .key nome della query nel file INI
#' @return un character array contenente la query SQL
#' @export
#' @exportMethod sql_by_key

methods::setGeneric(
  "sql_by_key",
  function(x, .key, ...) {
    standardGeneric("sql_by_key")
  })

methods::setMethod(
  "sql_by_key",
  signature("SQLHelper"),
  function(x, .key, ...) {
    .sql_by_key(x, .key, ...)
  }
)
