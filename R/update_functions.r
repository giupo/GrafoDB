.updateFunctions <- function(x, con, tag=x@tag, msg="") {
  if(interactive()) cat("Update Functions...")
  ## passo la connessione perche' devono avere la stessa transazione
  ## non usare controllo di transazione qui
  functions <- x@functions
  timestamp <- x@timestamp
  autore <- whoami()
  helper <- x@helper
  df <- if(length(keys(functions))) {
    dbGetQuery(con, getSQLbyKey(
      helper, "GET_CHANGED_FORMULE",
      tag=tag,
      last_updated=as.numeric(timestamp)))
  } else {
    data.frame()
  }

  names.with.conflicts <- intersect(x@touched, as.character(df$name))
  registerDoMC(detectCores())
  
  names.updated <- setdiff(keys(x@functions), names.with.conflicts)
  if(length(names.updated)) {
    formule <- foreach (name = iter(names.updated), .combine=rbind) %dopar% {
      formula <- expr(x, name, echo=FALSE)
      cbind(formula, autore, name, tag)
    }

    if(dbExistsTable(con, paste0("formule_", tag)) ||
         class(con) == "SQLiteConnection") {
      foreach(row = iter(formule, 'row')) %do% {
        formularow <- row[,1]
        namerow <- row[,3]
       
        dbGetQuery(con, getSQLbyKey(
          helper, "UPDATE_FORMULE",
          tag=tag,
          autore=autore,
          formula=formularow,
          name=namerow,
          msg=msg,
          last_updated=R.utils::System$currentTimeMillis()))
      }
    }
    
    
    foreach(name = iter(names.updated)) %do% {
      formula <- expr(x, name, echo=FALSE)
      dbGetQuery(con, getSQLbyKey(
        helper, "UPSERT_FORMULE",
        formula=formula,
        autore=autore,
        name=name,
        tag=tag,
        msg=msg,
        last_updated=R.utils::System$currentTimeMillis()))
    }
  }
  removeFromRedis(x, x@touched)
  # doneWithCluster()
  if(interactive()) cat("Done.\n")
}
