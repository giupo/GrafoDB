.updateFunctions <- function(x, con, tag=x@tag) {
  if(interactive()) cat("Update Functions...")
  ## passo la connessione perche' devono avere la stessa transazione
  ## non usare controllo di transazione qui
  functions <- x@functions
  timestamp <- x@timestamp
  df <- if(length(keys(functions))) {
    params <- cbind(tag, timestamp)
    sql <- paste("select name from formule where tag = ? ",
                 "and last_updated::timestamp(0) > to_timestamp(?)")
    dbGetPreparedQuery(con, sql, bind.data = params)
  } else {
    data.frame()
  }
  
  names.with.conflicts <- intersect(x@touched, as.character(df$name))
  # cl <- initCluster()
  registerDoMC(detectCores())
  is.multi.process <- TRUE # !is.null(cl)
  #if(is.multi.process) {
  #  clusterStartWorking()
  #}

  autore <- whoami()
  
  names.updated <- setdiff(keys(x@functions), names.with.conflicts)
  if(length(names.updated)) {
    formule <- if(is.multi.process) {
      foreach (name = iter(names.updated), .combine=rbind) %dopar% {
        task <- expr(x, name, echo=FALSE)
        cbind(task, whoami(), name, tag)
      }
    } else {
      foreach (name = iter(names.updated), .combine=rbind) %do% {
        task <- expr(x, name, echo=FALSE)
        cbind(task, whoami(), name, tag)
      }
    }
    
    if(dbExistsTable(con, paste0("formule_", tag))) {
      sql1 <- paste0("UPDATE formule_",tag,
                     " SET formula=?, autore=?, ",
                     " last_updated = LOCALTIMESTAMP::timestamp(0) ",
                     " WHERE name=? and tag=?");      
      dbGetPreparedQuery(con, sql1, bind.data=formule)
    }
    
    sql2 <- paste0(
      "INSERT INTO formule(formula, autore, name, tag, last_updated) ",
      " select ?,?,?,?,LOCALTIMESTAMP::timestamp(0) ",
      " WHERE NOT EXISTS (SELECT 1 FROM formule WHERE name=? and tag=?)")
    autore <- whoami()
    formule <- foreach (name = iter(names.updated), .combine=rbind) %do% {
      task <- expr(x, name, echo=FALSE)
      cbind(task, autore, name, tag, name, tag)
    }
    colnames(formule) <- c("formula", "autore", "name", "tag", "name", "tag")
    dbGetPreparedQuery(con, sql2, bind.data=formule)
  }
  removeFromRedis(x, x@touched)
  # doneWithCluster()
  if(interactive()) cat("Done.\n")
}
