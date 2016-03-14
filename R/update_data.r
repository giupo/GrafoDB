#' @include redis.r

.updateData <- function(x, con, tag=x@tag) {
  if(interactive()) cat("Update Data...")
  data <- x@data
  timestamp <- x@timestamp
  autore <- whoami()
  
  df <- if(length(keys(data))) {
    params <- cbind(tag, timestamp)
    sql <- paste("select name from dati where tag = ? ",
                 "and last_updated::timestamp(0) > to_timestamp(?)")
    
    dbGetPreparedQuery(con, sql, bind.data = params)
  } else {
    data.frame()
  }
  
  names.with.conflicts <- intersect(x@touched, as.character(df$name))
  ## aggiorno solo le serie cambiate
  
  names.updated <- setdiff(keys(data), names.with.conflicts)
  registerDoMC(detectCores())
  #cl <- initCluster()
  is.multi.process <- TRUE #!is.null(cl)
 # if(is.multi.process) {
 #   clusterStartWorking()
 # }
  autore <- whoami()
  if(length(names.updated)) { 
    dati <- if(is.multi.process) {
      foreach (name = iter(names.updated), .combine=rbind) %dopar% {
        df <- to.data.frame(data[[name]])
        cbind(df, autore, name, tag) 
      }
    } else {
      foreach (name = iter(names.updated), .combine=rbind) %do% {
        df <- to.data.frame(data[[name]])
        cbind(df, autore, name, tag)    
      }
    }
    
    
    if(dbExistsTable(con, paste0("dati_", tag))) {
      sql1 <- paste0("UPDATE dati_",tag,
                     " SET anno=?, periodo=?, freq=?, dati=?,",
                     "autore=?, last_updated = LOCALTIMESTAMP::timestamp(0) ",
                     " WHERE name=? and tag=?");
      dbGetPreparedQuery(con, sql1, bind.data=dati)
    }
    
    sql2 <- paste0(
      "INSERT INTO dati(anno, periodo, freq, dati, autore, ",
      " name, tag, last_updated) ",
      " select ?,?,?,?,?,?,?,LOCALTIMESTAMP::timestamp(0)",
      " WHERE NOT EXISTS (SELECT 1 FROM dati WHERE name=? and tag=?)")
    
    dati <- cbind(dati, names.updated, tag)
    dbGetPreparedQuery(con, sql2, bind.data=dati)
  }
  # doneWithCluster()
  removeFromRedis(x, x@touched)
  cat("Done.\n")
}
