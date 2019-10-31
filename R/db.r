#' Setup the DB
#'
#' Interactively reads db parameters and stores into your home dir
#'
#' @name setupdb
#' @usage setupdb()
#' @param overwrite if a config file exists, setting this
#'                  to `FALSE` raises an Exception
#' @importFrom stringr str_trim
#' @export

setupdb <- function(overwrite=FALSE) { # nocov start

  fileName <- file.path(path.expand("~"), ".GrafoDB/GrafoDB.ini")
  if((!overwrite) && file.exists(fileName)) {
    stop(fileName, " already exists and overwrite args was set to FALSE")
  }
  driver <- str_trim(readline(prompt="Driver DB (default: 'PostgreSQL'): "))
  if(driver == "") {
    driver <- "PostgreSQL"
  }
  host <- str_trim(readline(prompt="Host Address (default: 'localhost'): "))
  if(host == "") {
    host <- "localhost"
  }

  port <- str_trim(readline(prompt="Listening Port (default: '5432'): "))
  if(port == "") {
    port <- "5432"
  }

  dbname <- str_trim(readline(prompt="Database name (default: 'grafo'): "))
  if(dbname == "") {
    dbname <- "grafo"
  }

  username <- str_trim(readline(prompt="Username: "))

  password <- if(!is.windows()) {
    getPass <- function(prompt = "Password:"){
      cat(prompt)
      pass <- system('stty -echo && read ff && stty echo && echo $ff && ff=""',
                     intern=TRUE)
      cat('\n')
      invisible(pass)
    }
    getPass()
  } else {
    getPass <- function() {
      if(!requireNamespace("tcltk", quietly=TRUE)) {
        stop("can't find tcl/tk")
      }
      wnd <- tktoplevel()
      tclVar("") -> passVar
      # Label
      tkgrid(tklabel(wnd,text="Enter password:"))
      # Password box
      tkgrid(tkentry(wnd, textvariable=passVar,show="*")->passBox)
      # Hitting return will also submit password
      tkbind(passBox,"<Return>",function() tkdestroy(wnd))
      # OK button
      tkgrid(tkbutton(wnd,text="OK",command=function() tkdestroy(wnd)))
      # Wait for user to click OK
      tkwait.window(wnd)
      tclvalue(passVar)
    }
    getPass()
  }
  password <- str_trim(password)

  config <- list()
  config[["driver"]] <- driver
  config[["host"]] <- host
  config[["port"]] <- port
  config[["dbname"]] <- dbname

  if (username != "") {
    config[["username"]] <- username
  }
  if (password != "") {
    config[["password"]] <- username
  }
  buffer <- c("[ConnectionInfo]")
  for(nameitem in names(config)) {
    buffer <- c(buffer, paste0(nameitem, "=", config[[nameitem]]))
  }

  buffer <- paste(buffer, sep="\n")
  fileName <- file.path(path.expand("~"), ".GrafoDB/GrafoDB.ini")
  dir.create(dirname(fileName), showWarnings=FALSE, mode="0700")
  write(buffer, file=fileName, append=FALSE)
} # nocov end

#' Reads db connections settings
#'
#' Tryes :
#'   - `$HOME/.GrafoDB/GrafoDB.ini` file
#'   - Internal settings.
#'
#' @name dbSettings
#' @usage dbSettings()
#' @param flush if `TRUE` removes any option saved in the current
#'              session and reloads the settings
#' @return a list containing the infos used to connect via DBI
#' @importFrom rutils ini_parse
#' @importFrom futile.logger flog.info flog.debug flog.trace
#' @export

dbSettings <- function(flush=FALSE) {
  ln <- "GrafoDB.db.dbSettings"
  if(flush) {
    flog.trace("Flushing settings", name=ln)
    options(dbSettings=NULL)
  }

  settings <- getOption("dbSettings", NULL)

  if(is.null(settings)) {
    flog.trace("settings are null", name=ln)
    home_ini_file <- file.path(path.expand("~"), ".GrafoDB/GrafoDB.ini")
    flog.debug("Ini file: %s", home_ini_file, name=ln)
    if(file.exists(home_ini_file)) {
      flog.debug("%s esiste! lo parso", home_ini_file)
      home_settings <- ini_parse(home_ini_file)
      options(dbSettings=home_settings)
      flog.debug("settings: %s", home_settings, name=ln, capture=TRUE)
      return(home_settings)
    }

    flog.debug("Reverting to system wide INI", name=ln)
    filename <- file.path(system.file(package="GrafoDB"), "ini/GrafoDB.ini")
    flog.debug("File path for system wide INI: %s%", filename, name=ln)
    settings <- ini_parse(filename)
    flog.debug("Settings: %s", settings, name=ln, capture=TRUE)
    options(dbSettings=settings)
  }
  #SQLHelperType_ <- settings[[paste0("ConnectionInfo_", env)]]$driver

  #flog.trace("SQLHelper type in dbSettings: %s", SQLHelperType_, name=ln)
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
  flog.debug("enviroment setting: %s", xx, name=ln)
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
  flog.debug("Schema file name: %s", schemaFileName, name=ln)
  file <- file.path(system.file(package="GrafoDB"), "sql", schemaFileName)
    if(!file.exists(file)) {
    flog.error("Schema file doesn't exists: %s", file, name=ln)
    stop("Schema file doesn't exists: ", file)
  }
  flog.debug("Schema filename full path: %s", file, name=ln)
  file
}

#' @importFrom stringr str_split str_trim
#' @importFrom futile.logger flog.debug flog.error flog.info flog.warn

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
  flog.debug("Current env is '%s'", env, name=ln)
  file <- schemaFileFromEnv(env=env)
  sql <- paste(readLines(file), collapse="\n")

  statements <- str_split(sql, ";")[[1]]

  tryCatch({
    dbBegin(con)
    for(stm in statements) {
      stm <- str_trim(as.character(stm))
      if(nchar(stm) > 0) {
        flog.trace("%s", stm, name=ln)
        dbExecute(con, stm)
      }
    }
    dbCommit(con)
  }, error=function(cond) {
    dbRollback(con)
    stop(cond)
  })
}

#' Factory di connessioni al database
#'
#' @name buildConnection
#' @note Funzione interna
#' @rdname buildConnection-internal
#' @importFrom DBI dbDriver dbConnect
#' @importFrom futile.logger flog.error

buildConnection <- function(env = getenv()) {
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
    flog.error("Unknown env: %s, set GRAFODB_ENV variable to the correct value (prod/test)", env, name=ln)
    stop("Unknown env: ", env)
  }

  con
}


PostgresConnect <- function() {
  if(!requireNamespace("RPostgreSQL", quietly=TRUE)) {
    stop("install package RPostgreSQL with 'install.packages(\"RPostgreSQL\")")
  }
  dbConnect(RPostgreSQL::PostgreSQL())
}

SQLiteConnect <- function() {
  if (! requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Please install RSQLite: install.packages('RSQLite')")
  }
  drv <- RSQLite::dbDriver("SQLite")
  dbConnect(drv, dbname=":memory:")
}

# nocov start
#' @importFrom DBI dbExecute
.dbBeginPG <- function(conn) {
  ln <- "GrafoDB.db"
  flog.trace("start transaction", name=ln)
  dbExecute(conn, "START TRANSACTION")
  TRUE
}
# nocov end # can't check this code without production environment

.dbBeginSQLite <- function(conn) {
  ln <- "GrafoDB.db"
  flog.trace("start transaction", name=ln)
  dbExecute(conn, "BEGIN")
  TRUE
}


shouldCreateSchema  <- function(con) {
  tryCatch({
    df <- dbReadTable(con, "grafi")
    !is.data.frame(df)
  }, error=function(cond) {
    TRUE
  }, warning=function(cond) {
    TRUE
  })
}
