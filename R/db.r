#' trying to behave like a connection pool
#' (with a single connection :( )
#'
#' @name pgConnect
#' @usage pgConnect()
#' @return a Connection to Postgresql
#' @note this stores the connection into options and retrieves it back
#' @import rcf
#' @export

pgConnect <- function() {
  con <- if(is.null(getOption("pgConnect", NULL))) {
    rcf::pgConnect()
  } else {
    getOption("pgConnect")
  }
  
  con <- tryCatch({
    dbGetInfo(con)
    con
  }, error = function(err) {
    rcf::pgConnect()
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

dbSettings <- function() {
  settings <- getOption("dbSettings", NULL)
  if(is.null(settings)) {
    settings <-   ini_parse(file.path(system.file(package="rcf"),
                                      "ini/sql.ini"))$PostgreSQL
    settings$port <- "5432"
  }
  settings
}
