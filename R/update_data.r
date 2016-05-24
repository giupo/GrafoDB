#' @include redis.r conflicts.r

.updateData <- function(x, con, tag=x@tag) {
  if(interactive()) cat("Update Data...")
  data <- x@data
  helper <- x@helper
  timestamp <- x@timestamp
  autore <- whoami()
  
  df <- if(length(keys(data))) {
    dbGetQuery(con, getSQLbyKey(
      helper, "GET_CHANGED_DATA",
      tag=tag,
      last_updated=as.numeric(timestamp)))
  } else {
    data.frame()
  }
  
  names.with.conflicts <- intersect(x@touched, as.character(df$name))
  names.updated <- setdiff(keys(data), names.with.conflicts)
  
  registerDoMC(detectCores())

  if(length(names.updated)) { 
    dati <- foreach (name = iter(names.updated), .combine=rbind) %dopar% {
      df <- to.data.frame(data[[name]])
      cbind(df, autore, name, tag) 
    }
    
    if(dbExistsTable(con, paste0("dati_", tag))) {
      foreach(row = iter(dati, 'row')) %do% {
        name <- row$name
        anno <- row$anno
        periodo <- row$periodo
        freq <- row$freq
        datirow <- row$dati

        sql1 <- getSQLbyKey(
          helper, "UPDATE_DATI",
          anno=anno,
          periodo=periodo,
          freq=freq,
          dati=datirow,
          autore=autore,
          name=name,
          tag=tag)
  
        dbGetQuery(con, sql1)
      }
    }
    
    foreach(row = iter(dati, 'row')) %do% {
      name <- row$name
      anno <- row$anno
      periodo <- row$periodo
      freq <- row$freq
      datirow <- row$dati

      sql2 <- getSQLbyKey(
        helper, "UPSERT_DATI",
        anno=anno,
        periodo=periodo,
        freq=freq,
        dati=datirow,
        autore=autore,
        name=name,
        tag=tag)
        
      dbGetQuery(con, sql2)
    }
  }
  
  if (interactive()) cat("Done.\n")
}
