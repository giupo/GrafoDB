#' @include redis.r conflicts.r
#' @importFrom R.utils System
#' @importFrom futile.logger flog.info
#' @importFrom stringi stri_rand_strings
#' @importFrom DBI dbWriteTable

.updateData <- function(x, con, tag=x@tag, notes="") {
  ln <- "GrafoDB::updateData"

  if(interactive()) flog.info("Update Data ...", name=ln)

  data <- x@data
  helper <- x@helper
  autore <- whoami()

  df <- if(length(keys(data))) {
    dbGetQuery(con, getSQLbyKey(
      helper, "GET_CHANGED_DATA",
      tag=tag,
      last_updated=as.numeric(x@timestamp)))
  } else {
    data.frame()
  }
  
  names.with.conflicts <- intersect(x@touched, as.character(df$name))
  names.updated <- setdiff(keys(data), names.with.conflicts)

  if(length(names.updated)) {
    # stage_name <- paste0("s", stri_rand_strings(1, 8))
    stage_name <- "stage"
    # create temporary data with names_updated

    # creo il data.frame dei dati da usare nel DB
    tryCatch({
      last_updated <- time.in.millis()
      dati <- foreach (name = iter(names.updated), .combine=rbind) %dopar% {
        df <- to.data.frame(data[[name]])
        cbind(df, name, tag, autore, notes, last_updated)
      }  # this is quite fast, let's ignore the Progressbar here...
      
      dbExecute(con, getSQLbyKey(helper, "CREATE_STAGE", stage_name = stage_name))
      on.exit({
        tryCatch({
          dbExecute(con, getSQLbyKey(helper, 'DROP_STAGE', stage_name=stage_name))
        }, error=function(cond) {
          flog.warn(cond, name=ln)
        })
      })
      dbWriteTable(con, stage_name, dati, row.names=FALSE, overwrite=TRUE)
      # aggiorna i record esistenti...
      dbExecute(con, getSQLbyKey(
        helper, "UPDATE_WITH_STAGE",
        tag=tag,
        stage_name=stage_name)) 
      dbExecute(con, getSQLbyKey(
        helper, "INSERT_WITH_STAGE",
        tag = tag,
        stage_name=stage_name))
    }, error=function(cond) {
      stop(cond)
    })
  }  
  if (interactive()) {
    flog.info("Update Data done.", name=ln)
  }
}
