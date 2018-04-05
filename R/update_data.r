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
    dati <- foreach (name = iter(names.updated), .combine=rbind) %dopar% {
      df <- to.data.frame(data[[name]])
      cbind(df, name, tag) 
    }  # this is quite fast, let's ignore the Progressbar here...

    # non mi e' chiaro perche' facciamo sta doppia passata di update e upsert
    if(interactive()) {
      pb <- ProgressBar(min=1, max=length(names.updated))
    }
    count <- 1
    foreach(row = iter(dati, 'row')) %do% {
      name <- as.character(row$name)
      if(interactive()) updateProgressBar(pb, count, name)
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
        tag=tag,
        msg=msg,
        last_updated=R.utils::System$currentTimeMillis())
      
      rcExecute <- dbExecute(con, sql1)
      count <- count + 1
      ret <- list()
      ret[[name]] <- rcExecute
      ret
    }
    
    if(interactive()) {
      kill(pb)
      pb <- ProgressBar(min=1, max=length(names.updated))
    }
    
    count <- 1
    foreach(row = iter(dati, 'row')) %do% {
      name <- as.character(row$name)
      if(interactive()) updateProgressBar(pb, count, name)
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
        tag=tag,
        msg=msg,
        last_updated=R.utils::System$currentTimeMillis())
      
      rcExecute <- dbExecute(con, sql2)
      count <- count + 1
      ret <- list()
      ret[[name]] <- rcExecute
      ret
    }

    if (interactive()) {
      kill(pb)
    }
  }
  
  if (interactive()) {
    flog.info("Update Data done.", name=ln)
  }
}
