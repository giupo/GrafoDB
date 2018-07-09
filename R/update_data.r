#' @include redis.r conflicts.r
#' @importFrom R.utils System
#' @importFrom futile.logger flog.info
.updateData <- function(x, con, tag=x@tag, msg="") {
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
    
    # create temporary data with names_updated
    dbExecute(con, getSQLbyKey(helper, "CREATE_STAGE"))
    on.exit({
      dbExecute(con, getSQLbyKey(helper, "DROP_STAGE"))
    })

    # creo il data.frame dei dati da usare nel DB
    last_updated = R.utils::System$currentTimeMillis()
    dati <- foreach (name = iter(names.updated), .combine=rbind) %dopar% {
      df <- to.data.frame(data[[name]])
      # cbind(df, name, tag)
      cbind(df, name, tag, autore, msg, last_updated)
    }  # this is quite fast, let's ignore the Progressbar here...

    dbWriteTable(con, "stage", dati, row.names=FALSE, overwrite=TRUE)
    # aggiorna i record esistenti...
    dbExecute(con, getSQLbyKey(helper, "UPDATE_WITH_STAGE"))
    
    # ...ed inserisci i nuovi
    dbExecute(con, getSQLbyKey(helper, "DELETE_STAGE"))
    # ...non inserisco quelli gia' presenti
    dbDati <- loadDati(tag, con=con)
    dati_insert <- dati[!dati$name %in% dbDati$name, ]
    # e vado a scrivere stage...
    dbWriteTable(con, "stage", dati_insert, row.names=FALSE, overwrite=TRUE)
    # ... da usare nell'insert
    dbExecute(con, getSQLbyKey(helper, "INSERT_WITH_STAGE"))
  }
  
  if (interactive()) {
    flog.info("Update Data done.", name=ln)
  }
}
