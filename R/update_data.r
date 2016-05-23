#' @include redis.r conflicts.r

.updateData <- function(x, con, tag=x@tag) {
  if(interactive()) cat("Update Data...")
  data <- x@data
  timestamp <- x@timestamp
  autore <- whoami()
  
  df <- if(length(keys(data))) {
    sql <- paste0(
      "select name from dati where tag = '", tag, "' ",
      "and last_updated::timestamp(0) > to_timestamp(",
      as.numeric(timestamp), ")")
    
    dbGetQuery(con, sql)
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
        sql1 <- paste0(
          "UPDATE dati_",tag,
          " SET anno=", anno,", periodo=", periodo, ", freq=", freq,
          ", dati='", datirow, "',", "autore='", autore,"', ",
          "last_updated = LOCALTIMESTAMP::timestamp(0) ",
          " WHERE name='", name, "' and tag='", tag, "'")
        dbGetQuery(con, sql1)
      }
    }
    
    foreach(row = iter(dati, 'row')) %do% {
      name <- row$name
      anno <- row$anno
      periodo <- row$periodo
      freq <- row$freq
      datirow <- row$dati
      sql2 <- paste0(
        "INSERT INTO dati(anno, periodo, freq, dati, autore, ",
        " name, tag, last_updated) ",
        " select ",anno,", ", periodo, ",", freq, ",'", datirow, "','", autore ,"'",
        ",'", name, "','", tag,"', LOCALTIMESTAMP::timestamp(0)",
        " WHERE NOT EXISTS (SELECT 1 FROM dati WHERE name='", name,
        "' and tag='", tag, "')")

      dbGetQuery(con, sql2)
    }
  }
  
  if (interactive()) cat("Done.\n")
}
