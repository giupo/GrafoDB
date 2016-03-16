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

setupdb <- function(overwrite=FALSE) {

  fileName <- file.path(path.expand("~"), ".GrafoDB/GrafoDB.ini")
  if((!overwrite) && file.exists(fileName)) {
    stop(fileName, " already exists and overwrite args was set to FALSE")
  }
  driver <- str_trim(readline(prompt="Driver DB (default: 'PostgreSQL'): "))
  if(driver == "") {
    driver <- "PostgreSQL"
  }
  host <- str_trim(readline(prompt="Host Address (default: 'osi2-virt-001'): "))
  if(host == "") {
    host <- "osi2-virt-001"
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
    getPass <- function(){  
      require(tcltk);  
      wnd<-tktoplevel()
      tclVar("")->passVar;  
      #Label  
      tkgrid(tklabel(wnd,text="Enter password:"))
      #Password box  
      tkgrid(tkentry(wnd, textvariable=passVar,show="*")->passBox)
      #Hitting return will also submit password  
      tkbind(passBox,"<Return>",function() tkdestroy(wnd))
      #OK button  
      tkgrid(tkbutton(wnd,text="OK",command=function() tkdestroy(wnd)))
      #Wait for user to click OK  
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
  
  if(username != "") {
    config[["username"]] <- username
  }
  if(password != "") {
    config[["password"]] <- username
  }
  buffer = c("[ConnectionInfo]")
  for(nameitem in names(config)) {
    buffer <- c(buffer, paste0(nameitem, "=", config[[nameitem]]))
  }
  
  buffer <- paste(buffer, sep="\n")
  fileName <- file.path(path.expand("~"), ".GrafoDB/GrafoDB.ini")
  dir.create(dirname(fileName), showWarnings=FALSE, mode="0700")
  write(buffer, file=fileName, append=FALSE)
}

#' Reads db connections settings
#'
#' Tryes :
#'   - `$HOME/.GrafoDB/GrafoDB.ini` file
#'   - Internal settings.
#'
#' @name dbSettings
#' @usage dbSettings()
#' @param flush if `TRUE` removes any option recored in the current
#'              session and reloads the settings
#' @return a list containing the infos used to connect via DBI/RPostgreSQL
#' @importFrom rutils ini_parse
#' @export

dbSettings <- function(flush=FALSE) {
  
  if(flush) {
    options(dbSettings=NULL)
  }
  
  settings <- getOption("dbSettings", NULL)
  if(is.null(settings)) {
    home_ini_file <- file.path(path.expand("~"), ".GrafoDB/GrafoDB.ini")
    if(file.exists(home_ini_file)) {
      home_settings <- ini_parse(home_ini_file)
      if("ConnectionInfo" %in% names(home_settings)) {
        home_settings <- home_settings$ConnectionInfo
      }
      options(dbSettings=home_settings)
      return(home_settings)
    }
    
    settings <- ini_parse(
      file.path(system.file(package="GrafoDB"), "ini/sql.ini"))
    settings <- settings$ConnectionInfo
    options(dbSettings=settings)
  }
  settings
}

initdb <- function(con) {
  settings <- dbSettings()
  schemaFileName <- paste0("schema-", settings$driver, ".sql")
  file <- file.path(system.file(package="GrafoDB"), "sql", schemaFileName)
  sql <- paste(readLines(file), collapse="\n")
  dbGetQuery(con, sql)
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
#' @import RSQLite
#' @import RPostgreSQL

.buildConnection <- function(userid=whoami(), password=flypwd()) {
  settings <- dbSettings()
  drv <- dbDriver(settings$driver)
  
  if(settings$driver == "SQLite") {
    con <- dbConnect(drv, dbname=settings$dbname)
    if(settings$dbname == ":memory:") {
      initdb(con)
    }
    options(pgConnect=con)
    return(con)
  } 

  drv <- dbDriver(settings$driver)
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
    message("no kerberos ticket, fallback to userid/pwd")
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
  }
}


#' trying to behave like a connection pool
#' (with a single connection :( )
#'
#' @name pgConnect
#' @usage pgConnect()
#' @return a Connection to Postgresql
#' @note this stores the connection into options and retrieves it back
#' @importFrom DBI dbGetInfo
#' @export

pgConnect <- function(env="prod", userid=NULL, password=NULL, con=NULL) {
  if(!is.null(con)) {
    return(con)
  }
  
  con <- getOption("pgConnect", NULL)
  ## veriifico la connessione
  con <- if(is.null(con)) {
    .buildConnection(userid, password)
  } else {
    tryCatch({
      dbGetInfo(con)
      con
    }, error = function(err) {
      .buildConnection(userid, password)
    })
  }
  
  options(pgConnect=con)
  con
}
  
#' overrides dbDisconnect to do nothings in case pgConnect is active in options
#'
#' @name dbDisconnect
#' @param con connection to be closed
#' @importFrom RPostgreSQL dbDisconnect
#' @export

dbDisconnect <- function(con) {
  if(is.null(getOption("pgConnect", NULL))) {
    DBI::dbDisconnect(con)
  } else {
    TRUE
  }
}
