.updateFunctions <- function(x, con, tag=x@tag) {
  if(interactive()) cat("Update Functions...")
  ## passo la connessione perche' devono avere la stessa transazione
  ## non usare controllo di transazione qui
  functions <- x@functions
  timestamp <- x@timestamp
  autore <- whoami()
  
  df <- if(length(keys(functions))) {
    params <- cbind(tag, timestamp)
    sql <- paste0("select name from formule where tag = '", tag, "' ",
                 "and last_updated::timestamp(0) > to_timestamp(", as.numeric(timestamp), ")")
    dbGetQuery(con, sql)
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

    if(dbExistsTable(con, paste0("formule_", tag))) {
      foreach(row = iter(formule, 'row')) %do% {
        formularow <- row[,1]
        namerow <- row[,3]
        
        sql1 <- paste0(
          "UPDATE formule_",tag,
          " SET formula='", formularow, "', autore='", autore, "', ",
          " last_updated = LOCALTIMESTAMP::timestamp(0) ",
          " WHERE name='", namerow,"' and tag='", tag, "'")
        dbGetQuery(con, sql1)
      }
    }


    foreach(name = iter(names.updated)) %do% {
      formula <- expr(x, name, echo=FALSE)
      sql2 <- paste0(
        "INSERT INTO formule(formula, autore, name, tag, last_updated) ",
        " select '", formula, "','", autore, "', '", name, "', '", tag,"', LOCALTIMESTAMP::timestamp(0) ",
        " WHERE NOT EXISTS (SELECT 1 FROM formule WHERE name='", name, "' and tag='", tag, "')")
      
      dbGetQuery(con, sql2)
    }
  }
  removeFromRedis(x, x@touched)
  # doneWithCluster()
  if(interactive()) cat("Done.\n")
}
