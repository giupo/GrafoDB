#' Questa classe provvede a disaccoppiare SQL dal codice dell'app
#'
#' Separa le query SQL con delle chiavi, riponendo il linguaggio SQL in dei file
#' editabili e accedendo alle singole query con delle chiavi.
#'
#' Le query vengono inizializzate in un file .ini
#'
#' @name SQLHelper-class
#' @usage SQLHelper(...)
#' @slot sqlContainer a list of key-values for the SQL queries
#' @slot type the type of syntax
#' @export SQLHelper
#' @exportClass SQLHelper
#' @include logging.r

SQLHelper <- setClass( # nolint
  "SQLHelper",
  methods::representation(
    sqlContainer = "list",
    type = "character"
  ))

methods::setMethod(
  "initialize",
  signature("SQLHelper"),
  function(.Object, type = NULL) { # nolint
    init_sql_helper(.Object, type = type) #nolint
  })



sql_helper_type_by_env <- function(env = getenv()) {
  rutils::ifelse(env == "test", "SQLite", "PostgreSQL")
}

#' initializes the SQL helper
#'
#' @name init_sql_helper
#' @usage init_sql_helper(object)
#' @usage init_sql_helper(object, type
#' @param object the SQLHelper instance
#' @param type the kind of SQLhelper to initialize.
#'    Depends on environment (`getenv`) if not passwd by
#' @return the object initialized

init_sql_helper <- function(object, type = NULL) {
  env <- getenv()

  type <- rutils::ifelse(is.null(type),
    sql_helper_type_by_env(env = env),
    type)

  object@type <- type
  trace("SQLHeleperType: %s", type, name = "GrafoDB.sqlhelper")

  ini_file_path <- file.path(
    system.file(package = "GrafoDB", mustWork = TRUE),
    "ini/sql.ini")

  trace("SQLHelper INI path: %s", ini_file_path)
  object@sqlContainer <- rutils::ini_parse(ini_file_path)[[object@type]]

  object
}


assert_sql_params <- function(sql) {
  idx <- stringr::str_locate_all(sql, "--[A-Z|a-z|0-9]*--")[[1]]

  if (length(idx) == 0) return(invisible(TRUE))

  for (irow in seq_len(nrow(idx))) {
    start <- idx[irow, 1]
    end <- idx[irow, 2]
    param <- substring(sql, first = start, last = end)
    warning(param, " has not been set")
  }
  stop("params for query ", sql, " have not been set")
}


sql_by_key_impl <- function(x, .key, ...) {
  sqlContainer <- x@sqlContainer # nolint
  if (! .key %in% names(sqlContainer)) {
    stop(.key, " not in query repository")
  }

  ## retrieve sql
  sql <- sqlContainer[[.key]]

  ## handle param list
  params <- list(...)

  for (name in names(params)) {
    param_key <- paste0("--", name, "--")
    value <- params[[name]]
    if (is.null(value)) {
      value <- ""
    }
    quoted_value <- gsub("'", "''", value)[]
    sql <- gsub(param_key, quoted_value, sql)
  }

  ## check if any params is left behind
  tryCatch(assert_sql_params(sql), error = function(cond) {
    stop("params for query ", .key, " of type ",
      x@type, " have not been set: ", cond)
  })

  trace("Query for key '%s' = %s", .key, sql, name = "GrafoDB.sqlhelper")
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
#' @param x istanza di SQLHelper
#' @param .key nome della query nel file INI
#' @return un character array contenente la query SQL
#' @export
#' @exportMethod sql_by_key
#' @docType methods
#' @rdname sql_by_key-methods

methods::setGeneric(
  "sql_by_key",
  function(x, .key, ...) {
    standardGeneric("sql_by_key")
  })

#' @rdname sql_by_key-methods
#' @aliases SQLHelper,character,ANY-method

methods::setMethod(
  "sql_by_key",
  signature("SQLHelper", "character"),
  function(x, .key, ...) {
    sql_by_key_impl(x, .key, ...)
  }
)
