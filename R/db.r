dbSettings <- function() {
  settings <- getOption("dbSettings", NULL)
  if(is.null(settings)) {
    settings <-   ini_parse(file.path(system.file(package="GrafoDB"),
                                      "ini/sql.ini"))$PostgreSQL
    settings$port <- "5432"
  }
  settings
}

.buildConnection <- function(userid=NULL, password=NULL) {
  settings <- dbSettings()
  drv <- dbDriver(settings$driver)
  con <- tryCatch(
    dbConnect(drv, host=settings$host, dbname=settings$dbname),
    error = function(cond) {
      NULL
    })
  
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
    c
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
#' @import rutils
#' @export

pgConnect <- function(env="prod", userid=NULL, password=NULL) {
  con <- getOption("pgConnect", NULL)
  ## veriifico la connessione
  con <- tryCatch({
    dbGetInfo(con)
    con
  }, error = function(err) {
    .buildConnection(userid, password)
  })
  
  options(pgConnect=con)
  con
}
  
#' overrides dbDisconnect to do nothings in case pgConnect is active in options
#'
#' @name dbDisconnect
#' @param con connection to be closed
#' @import DBI
#' @export

dbDisconnect <- function(con) {
  if(is.null(getOption("pgConnect", NULL))) {
    RPostgreSQL::dbDisconnect(con)
  } else {
    TRUE
  }
}
