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
      if(!requireNamespace(tcltk, quietly=TRUE)) {
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
  settings <- dbSettings()
  settings <- settings[[paste0("ConnectionInfo_", env)]]
  schemaFileName <- paste0("schema-", settings$driver, ".sql")
  flog.debug("Schema file name: %s", schemaFileName, name=ln)
  
  file <- file.path(system.file(package="GrafoDB"), "sql", schemaFileName)
  flog.debug("Schema filename full path: %s", file, name=ln)
  file
}

#' @importFrom stringr str_split str_trim
#' @importFrom futile.logger flog.debug flog.error flog.info flog.warn

initdb <- function(con) {
  ln <- "GrafoDB.db.initdb"
  env <- getenv()
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

#' Factory di connessioni al database Postgresql
#'
#' @name .buildConnection
#' @note Funzione interna
#' @rdname buildConnection-internal
#' @param userid userid utente
#' @param password password utente (defaults to flypwd)
#' @importFrom rutils whoami flypwd
#' @importFrom DBI dbDriver dbConnect

.buildConnection <- function(userid=whoami(), password=flypwd()) {
  ln <- "GrafoDB.db"
  settings <- dbSettings()
  
  env <- getenv()
  
  settings <- settings[[paste0("ConnectionInfo_", env)]]  

  flog.debug("Partial Settings: %s", settings)
  
  if(settings$driver == "SQLite") {
    if (! requireNamespace("RSQLite", quietly = TRUE)) {
      stop("Please install RSQLite: install.packages('RSQLite')")
    }
    require(RSQLite)
    drv <- RSQLite::dbDriver(settings$driver)
    con <- dbConnect(drv, dbname=settings$dbname)
    if(settings$dbname == ":memory:") {
      initdb(con)
    }
   #  options(pgConnect=con)
    return(con)
  } 

  if (! requireNamespace("RPostgreSQL", quietly = TRUE)) {
    stop("Please install RPostgreSQL: install.packages('RPostgreSQL')")
  } else {
    require(RPostgreSQL)
  }
  
  drv <- dbDriver(settings$driver) #nocov start
  con <- tryCatch(
    dbConnect(drv, host=settings$host, dbname=settings$dbname),
    error = function(cond) {
      NULL
    })

  if(is.null(con)) {
    ## refresh kerberos ticket
    system("flypwd -p | kinit > /dev/null")  
    con <- tryCatch(
      dbConnect(drv, host=settings$host, dbname=settings$dbname),
      error = function(cond) {
        NULL
      })
  }
  
  if(is.null(con)) {
    flog.warn("no kerberos ticket, fallback to userid/pwd", name=ln)
    userid <- if(is.null(userid)) {
      whoami()
    } else {
      userid
    }
    
    password <- if(is.null(password)) {
      flypwd()
    } else {
      password
    }
    
    dbConnect(drv, user=userid, password=password,
              host=settings$host, dbname=settings$dbname)
  } else {
    con
  } # nocov end # can't check this code without production environment
}


#' trying to behave like a connection pool
#' (with a single connection :( )
#'
#' @name pgConnect
#' @usage pgConnect()
#' @return a Connection to Postgresql
#' @note this stores the connection into options and retrieves it back
#' @importFrom DBI dbGetQuery
#' @export

pgConnect <- function(userid=NULL, password=NULL, con=NULL) {
  ln <- "GrafoDB.db"
  flog.trace(msg="pgConnect", name=ln)
  if(!is.null(con)) {
    return(con)
  }
  
  con <- getOption("pgConnect", NULL)
  ## veriifico la connessione
  con <- if(is.null(con)) {
    flog.debug("Con is null, building connection...", name=ln)
    .buildConnection(userid, password)
  } else {
    tryCatch({
      ## dbGetInfo(con) e' deprecato. faccio il check su "grafi"
      grafi <- dbGetQuery(con, "select * from grafi");
      if (!is.data.frame(grafi)) {
        flog.warn("Grafi doesn't exists on DB, rebuilding connection...", name=ln)
        .buildConnection(userid, password) # nocov
      } else {
        con
      }
    }, error = function(err) {
      flog.warn("Got an error in pgConnect, rebuilding connection...", name=ln)
      .buildConnection(userid, password) # nocov
    })
  }
  
  options(pgConnect=con)
  con
}
  
#' overrides dbDisconnect to do nothings in case pgConnect is active in options
#'
#' @name dbDisconnect
#' @param con connection to be closed
#' @export

dbDisconnect <- function(con) {
  ln <- "GrafoDB.db"
  if(is.null(getOption("pgConnect", NULL))) {
    flog.trace("Connection was created outside GrafoDB, closing for real...", name=ln)
    DBI::dbDisconnect(con) # nocov
  } else {
    flog.trace("Connection was created inside GrafoDB, fake closing", name=ln)
    TRUE
  }
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
