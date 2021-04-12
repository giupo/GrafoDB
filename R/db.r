#' Reads db connections settings
#'
#' Tryes :
#'   - `$HOME/.GrafoDB/GrafoDB.ini` file
#'   - Internal settings.
#'
#' @name db_settings
#' @usage db_settings()
#' @param flush if `TRUE` removes any option saved in the current
#'              session and reloads the settings
#' @return a list containing the infos used to connect via DBI
#' @include logging.r
#' @export

db_settings <- function(flush=FALSE) {
  ln <- "GrafoDB.db.db_settings"
  if(flush) {
    trace("Flushing settings", name=ln)
    options(db_settings=NULL)
  }

  settings <- getOption("db_settings", NULL)

  if(is.null(settings)) {
    trace("settings are null", name=ln)
    home_ini_file <- file.path(path.expand("~"), ".GrafoDB/GrafoDB.ini")
    debug("Ini file: %s", home_ini_file, name=ln)
    if(file.exists(home_ini_file)) {
      debug("%s esiste! lo parso", home_ini_file)
      home_settings <- rutils::ini_parse(home_ini_file)
      options(db_settings=home_settings)
      debug("settings: %s", home_settings, name=ln, capture=TRUE)
      return(home_settings)
    }

    debug("Reverting to system wide INI", name=ln)
    filename <- file.path(system.file(package="GrafoDB"), "ini/GrafoDB.ini")
    debug("File path for system wide INI: %s%", filename, name=ln)
    settings <- rutils::ini_parse(filename)
    debug("Settings: %s", settings, name=ln, capture=TRUE)
    options(db_settings=settings)
  }
  #SQLHelperType_ <- settings[[paste0("ConnectionInfo_", env)]]$driver

  #flog.trace("SQLHelper type in db_settings: %s", SQLHelperType_, name=ln)
  #options(SQLHelperType=SQLHelperType_)

  settings
}


#' Returns the enviroment for GrafoDB
#'
#' Returns the value of environment variable `GRAFODB_ENV`
#'
#' @name getenv
#' @export

getenv <- function() {
  ln <- "GrafoDB.db.getenv"
  xx <- Sys.getenv("GRAFODB_ENV", "prod")
  debug("enviroment setting: %s", xx, name=ln)
  xx
}


#' Ritorna il nome del file contenente lo schema dei dati in base all' `env`
#'
#' `env` ottenuto con `getenv` e' la variabile d'ambiente GRAFODB_ENV, di
#' default settata a `prod`. Negli ambienti di produzione e' necessario cambiarla
#' a `test` e modificare accordingly la configurazione
#'
#' @name schemaFileFromEnv
#' @param env ambiente di riferimento (di default e' `getenv`)
#' @return percorso al file contenente lo schema dei dati
#' @note attualmente esistono solo due schemi, per SQLite (tipicamente `test`)
#'       e PostgreSQL (`prod`)

schemaFileFromEnv <- function(env = getenv()) {
  ln <- "GrafoDB.db.schemaFileFromEnv"

  driver <- if (env == "prod" || env == "collaudo") {
    "PostgreSQL"
  } else {
    "SQLite"
  }

  schemaFileName <- paste0("schema-", driver, ".sql")
  debug("Schema file name: %s", schemaFileName, name=ln)
  file <- file.path(system.file(package="GrafoDB"), "sql", schemaFileName)
    if(!file.exists(file)) {
    error("Schema file doesn't exists: %s", file, name=ln)
    stop("Schema file doesn't exists: ", file)
  }
  debug("Schema filename full path: %s", file, name=ln)
  file
}

#' @include logging.r

initdb <- function(con, env=getenv()) {
  if (env == "test") {
    initdbSQLite(con, env=env)
  } else {
    initdbPostgreSQL(env=env)
  }
}

initdbPostgreSQL <- function(env=getenv()) {
  file <- schemaFileFromEnv(env=env)
  dbname <- if (env != "prod") {
    "grafo_test"
  } else {
    # let the PGDBNAME set this.
    ""
  }
  system(paste0("psql ", dbname, " < ", file))
}

initdbSQLite <- function(con, env=getenv()) {
  ln <- "GrafoDB.db.initdbSQLite"
  debug("Current env is '%s'", env, name=ln)
  file <- schemaFileFromEnv(env=env)
  sql <- paste(readLines(file), collapse="\n")

  statements <- stringr::str_split(sql, ";")[[1]]

  tryCatch({
    DBI::dbBegin(con)
    for(stm in statements) {
      stm <- stringr::str_trim(as.character(stm))
      if(nchar(stm) > 0) {
        trace("%s", stm, name=ln)
        DBI::dbExecute(con, stm)
      }
    }
    DBI::dbCommit(con)
  }, error=function(cond) {
    DBI::dbRollback(con)
    stop(cond)
  })
}

#' Factory di connessioni al database
#'
#' @name buildConnection
#' @note Funzione interna
#' @rdname buildConnection-internal
#' @include logging.r
buildConnection <- function(env = getenv(), con = NULL) {
  if (!is.null(con)) return(con)

  ln <- "GrafoDB.buildConnection"
  con <- if(env == "test") {
    con <- getOption("GrafoDB_connection", NULL)
    if (is.null(con)) {
      con <- SQLiteConnect()
      options(GrafoDB_connection = con)
      con 
    } else {
      con
    }
  } else if (env == "prod") {
    PostgresConnect()
  } else {
    error("Unknown env: %s, set GRAFODB_ENV variable to the correct value (prod/test)", env, name=ln)
    stop("Unknown env: ", env)
  }


  if(env == "test" && shouldCreateSchema(con)) {
    initdb(con, env=env)
  }
  con
}


PostgresConnect <- function() {
  if(!requireNamespace("RPostgreSQL", quietly=TRUE)) {
    stop("install package RPostgreSQL with 'install.packages(\"RPostgreSQL\")")
  }
  DBI::dbConnect(RPostgreSQL::PostgreSQL())
}

SQLiteConnect <- function() {
  if (! requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Please install RSQLite: install.packages('RSQLite')")
  }
  drv <- RSQLite::dbDriver("SQLite")
  DBI::dbConnect(drv, dbname=":memory:")
}

# nocov start
.dbBeginPG <- function(conn) {
  ln <- "GrafoDB.db"
  trace("start transaction", name=ln)
  DBI::dbExecute(conn, "START TRANSACTION")
  TRUE
}
# nocov end # can't check this code without production environment

.dbBeginSQLite <- function(conn) {
  ln <- "GrafoDB.db"
  trace("start transaction", name=ln)
  DBI::dbExecute(conn, "BEGIN")
  TRUE
}


shouldCreateSchema  <- function(con) {
  tryCatch({
    df <- DBI::dbReadTable(con, "grafi")
    !is.data.frame(df)
  }, error=function(cond) {
    TRUE
  }, warning=function(cond) {
    TRUE
  })
}

#' proxy the dbDisconnect based on `getenv` value
#'
#' @name disconnect
#' @param con connection to disconnect
#' @param env environment to check against

disconnect <- function(con, env=getenv()) {
  if(env != "test") DBI::dbDisconnect(con)
}
