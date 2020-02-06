#' @importFrom futile.logger flog.info

.updateFunctions <- function(x, con, tag=x@tag, msg="") {
  ln <- "GrafoDB::updateFunctions"
  if(interactive()) flog.info("Update Functions ...", name=ln)

  ## passo la connessione perche' devono avere la stessa transazione
  ## non usare controllo di transazione qui
  functions <- x@functions
  timestamp <- x@timestamp
  autore <- rutils::whoami()
  helper <- x@helper
  df <- if(length(hash::keys(functions))) {
    dbGetQuery(con, getSQLbyKey(
      helper, "GET_CHANGED_FORMULE",
      tag=tag,
      last_updated=as.numeric(timestamp)))
  } else {
    data.frame()
  }

  names.with.conflicts <- intersect(x@touched, as.character(df$name))
  
  names.updated <- setdiff(hash::keys(x@functions), names.with.conflicts)
  if(length(names.updated)) {
    formule <- foreach::foreach (name = iter(names.updated), .combine=rbind) %dopar% {
      formula <- expr(x, name, echo=FALSE)
      cbind(formula, autore, name, tag)
    }

    if(DBI::dbExistsTable(con, paste0("formule_", tag)) ||
         class(con) == "SQLiteConnection") {
      foreach::foreach(row = iter(formule, 'row')) %do% {
        formularow <- row[,1]
        namerow <- row[,3]
       
        DBI::dbExecute(con, getSQLbyKey(
          helper, "UPDATE_FORMULE",
          tag=tag,
          autore=autore,
          formula=formularow,
          name=namerow,
          msg=msg,
          last_updated=time.in.millis()))
      }
    }
    
    
    foreach::foreach(name = iter(names.updated)) %do% {
      formula <- expr(x, name, echo=FALSE)
      DBI::dbExecute(con, getSQLbyKey(
        helper, "UPSERT_FORMULE",
        formula=formula,
        autore=autore,
        name=name,
        tag=tag,
        msg=msg,
        last_updated=time.in.millis()))
    }
  }
  removeFromRedis(x, x@touched)
  
  if(interactive()) flog.info("Update Functions done.", name=ln)
}
