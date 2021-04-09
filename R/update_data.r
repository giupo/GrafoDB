#' @include conflicts.r
#' @include logging.r

update_data <- function(x, con, tag = x@tag, notes = "") {
  ln <- "GrafoDB::update_data"

  if (interactive()) flog.info("Update Data ...", name = ln)

  data <- x@data
  helper <- x@helper
  autore <- rutils::whoami()

  df <- if (length(hash::keys(data))) {
    DBI::dbGetQuery(con, getSQLbyKey(
      helper, "GET_CHANGED_DATA",
      tag=tag,
      last_updated=as.numeric(x@timestamp)))
  } else {
    data.frame()
  }

  names_with_conflicts <- intersect(x@touched, as.character(df$name))
  names_updated <- setdiff(hash::keys(data), names_with_conflicts)

  if (length(names_updated)) {
    # stage_name <- paste0("s", stri_rand_strings(1, 8))
    stage_name <- "stage"
    # create temporary data with names_updated

    # creo il data.frame dei dati da usare nel DB
    tryCatch({
      last_updated <- time.in.millis()
      dati <- foreach::`%dopar%`(foreach::foreach(
        name = iterators::iter(names_updated), .combine=rbind), {
        df <-  to_data_frame(data[[name]])
        cbind(df, name, tag, autore, notes, last_updated)
      }) # this is quite fast, let's ignore the Progressbar here...

      DBI::dbExecute(con, getSQLbyKey(
        helper, "CREATE_STAGE", stage_name = stage_name))
      on.exit({
        tryCatch({
          DBI::dbExecute(con,
            getSQLbyKey(helper, 'DROP_STAGE', stage_name = stage_name))
        }, error=function(cond) {
          flog.warn(cond, name = ln)
        })
      })

      DBI::dbWriteTable(con, stage_name, dati,
        row.names = FALSE, overwrite = TRUE)

      # aggiorna i record esistenti...
      DBI::dbExecute(con, getSQLbyKey(
        helper, "UPDATE_WITH_STAGE",
        tag=tag,
        stage_name=stage_name))

      DBI::dbExecute(con, getSQLbyKey(
        helper, "INSERT_WITH_STAGE",
        tag = tag,
        stage_name=stage_name))
    }, error=function(cond) {
      stop(cond)
    })
  }
  if (interactive()) {
    flog.info("Update Data done.", name = ln)
  }
}
