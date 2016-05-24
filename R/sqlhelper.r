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
#' @importFrom rutils ini_parse
#' @importFrom stringr str_locate_all 

SQLHelper <- setClass(
  "SQLHelper",
  representation(
    sqlContainer="list",
    type="character"
  ))

setMethod(
  "initialize",
  signature("SQLHelper"),
  function(.Object, path = NULL, type="PostgreSQL") {
    .Object@type <- getOption("SQLHelperType", type)
    .Object@sqlContainer <- ini_parse(
      file.path(system.file(package="GrafoDB", mustWork=T),
                "ini/sql.ini"))[[.Object@type]]
    
    .Object
  })

#' Factory di query (stringhe) SQL.
#'
#' Ritorna le query SQL cosi' come storicizzate nell'SQLHelper. Sostituisce
#' eventuali parametri (key=value) della query nell'ellipsis del metodo
#'
#' Le query hanno una forma "select * from --tab-- where param='--value--'";
#' Il metodo si occupa di sostiture '--tab--' e '--value--' secondo i valori
#' passati come argomento: Es: `getSQLbyKey(helper, KEYQUERY, tab="A", value="B")`
#' risultera' in `select * from A where param='B'`
#' 
#' @name getSQLbyKey
#' @usage getSQLbyKey(x, key)
#' @usage getSQLbyKey(x, key, param=value, param2, value2, ...)
#' 
#' @param x istanza di SQLHelper
#' @param .key nome della query nel file INI
#' @return un character array contenente la query SQL
#' @export
#' @exportMethod getSQLbyKey

setGeneric(
  "getSQLbyKey",
  function(x, .key, ...) {
    standardGeneric("getSQLbyKey")
  })

setMethod(
  "getSQLbyKey",
  signature("SQLHelper"),
  function(x, .key, ...) {
    if(! .key %in% names(x@sqlContainer)) {
      stop(.key, " not in query repository")
    }

    ## retrieve sql
    sql <- x@sqlContainer[[.key]]

    ## handle param list
    params <- list(...)

    for(name in names(params)) {
      paramKey <- paste0("--", name, "--")
      value <- params[[name]]
      quotedValue <- gsub("'", "''", value)
      sql <- gsub(paramKey, quotedValue, sql)
    }

    ## check if any params is left behind

    idx <- str_locate_all(sql, "--[A-Z|a-z|0-9]*--")[[1]]
    if(length(idx) > 0) {
      for(irow in 1:nrow(idx)) {
        start <- idx[irow, 1]
        end <- idx[irow, 2]
        param <- substring(sql, first=start, last=end)
        warning(param, " has not been set")
      }
      stop("params for query ", .key, " of type ", x@type, " have not been set")
    }
    
    sql
  }
)
