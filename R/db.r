#' Returns the enviroment for GrafoDB
#'
#' Returns the value of environment variable `GRAFODB_ENV`
#'
#' @name getenv
#' @returns the value of the GRAFODB_ENV, if not specified, defaults
#'    to `prod`
#' @export

getenv <- function() {
  ln <- "GrafoDB.db.getenv"
  xx <- Sys.getenv("GRAFODB_ENV", "prod")
  debug("enviroment setting: %s", xx, name = ln)
  xx
}


#' Ritorna il nome del file contenente lo schema dei dati in base all' `env`
#'
#' `env` ottenuto con `getenv` e' la variabile d'ambiente GRAFODB_ENV, di
#' default settata a `prod`. Negli ambienti di produzione e' necessario
#' cambiarla
#' a `test` e modificare accordingly la configurazione
#'
#' @name schema_from_env
#' @param env ambiente di riferimento (di default e' `getenv`)
#' @return percorso al file contenente lo schema dei dati
#' @note attualmente esistono solo due schemi, per SQLite (tipicamente `test`)
#'       e PostgreSQL (`prod`)

schema_from_env <- function(env = getenv()) {
  ln <- "GrafoDB.db.schema_from_env"

  driver <- if (env == "prod" || env == "collaudo") {
    "PostgreSQL"
  } else {
    "SQLite"
  }

  schema_file_name <- paste0("schema-", driver, ".sql")
  debug("Schema file name: %s", schema_file_name, name = ln)
  file <- file.path(system.file(package = "GrafoDB"), "sql", schema_file_name)
    if (!file.exists(file)) {
    error("Schema file doesn't exists: %s", file, name = ln)
    stop("Schema file doesn't exists: ", file)
  }
  debug("Schema filename full path: %s", file, name = ln)
  file
}

#' @include logging.r

initdb <- function(con, env=getenv()) {
  if (env == "test") {
    initdb_sqlite(con, env = env)
  } else {
    initdb_postgres(env = env)
  }
}

initdb_postgres <- function(env=getenv()) {
  file <- schema_from_env(env = env)
  dbname <- if (env != "prod") {
    "grafo_test"
  } else {
    # let the PGDBNAME set this.
    ""
  }
  system(paste0("psql ", dbname, " < ", file))
}

initdb_sqlite <- function(con, env=getenv()) {
  ln <- "GrafoDB.db.initdb_sqlite"
  debug("Current env is '%s'", env, name = ln)
  file <- schema_from_env(env = env)
  sql <- paste(readLines(file), collapse = "\n")

  statements <- stringr::str_split(sql, ";")[[1]]

  tryCatch({
    DBI::dbBegin(con)
    for (stm in statements) {
      stm <- stringr::str_trim(as.character(stm))
      if (nchar(stm) > 0) {
        trace("%s", stm, name = ln)
        DBI::dbExecute(con, stm)
      }
    }
    DBI::dbCommit(con)
  }, error = function(cond) {
    DBI::dbRollback(con)
    stop(cond)
  })
}

#' Factory di connessioni al database
#'
#' @name build_connection
#' @note Funzione interna
#' @param env environment label
#' @rdname build_connection-internal
#' @include logging.r

build_connection <- function(env = getenv()) {
  ln <- "GrafoDB.build_connection"
  con <- if (env == "test") {
    con <- getOption("GrafoDB_connection", NULL)
    if (is.null(con)) {
      con <- sqlite_connect()
      options(GrafoDB_connection = con)
      con
    } else {
      con
    }
  } else if (env == "prod") {
    postgres_connect()
  } else {
    error("Unknown env: %s, set GRAFODB_ENV variable to the",
      "correct value (prod/test)",
      env, name = ln)
    stop("Unknown env: ", env)
  }


  if (env == "test" && should_create_schema(con)) {
    initdb(con, env = env)
  }
  con
}


#' Builds a connection to Postgres
#'
#' @name postgres_connect
#' @return a DBI connection to postgres
#' @note fails if RPostgreSQL is not installed
#'    the connection params are delegated to the libpq environment variables
#'    https://www.postgresql.org/docs/9.3/libpq-envars.html

postgres_connect <- function() {
  if (!requireNamespace("RPostgreSQL", quietly = TRUE)) {
    stop("install package RPostgreSQL with 'install.packages(\"RPostgreSQL\")")
  }
  DBI::dbConnect(RPostgreSQL::PostgreSQL())
}

#' builds a SQLite connection
#'
#' @name sqlite_connect
#' @return a connection to SQLite
#' @note this is used for testing, the connection is to an in memory instance

sqlite_connect <- function() {
  if (! requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Please install RSQLite: install.packages('RSQLite')")
  }
  DBI::dbConnect(RSQLite::dbDriver("SQLite"), dbname = ":memory:")
}

#' Returns true if the app should create the DB schema
#'
#' @name should_create_schema
#' @param con DB connection
#' @return `TRUE` if db schema creation is needed, `FALSE` otherwise

should_create_schema  <- function(con) {
  tryCatch({
    df <- DBI::dbReadTable(con, "grafi")
    !is.data.frame(df)
  }, error = function(cond) {
    TRUE
  }, warning = function(cond) {
    TRUE
  })
}

#' proxy the dbDisconnect based on `getenv` value
#'
#' if `env` is `test`, is a no-op.
#' Otherwise delegates the call to `DBI::dbDisconnect`
#'
#' @name disconnect
#' @param con connection to disconnect
#' @param env environment to check against

disconnect <- function(con, env = getenv()) {
  if (env != "test") DBI::dbDisconnect(con)
}
