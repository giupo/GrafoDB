#' @include logging.r

update_functions <- function(x, con, tag = x@tag, msg = "") {
  ln <- "GrafoDB::update_functions"
  if (interactive()) info("Update Functions ...", name = ln)

  ## passo la connessione perche' devono avere la stessa transazione
  ## non usare controllo di transazione qui
  functions <- x@functions
  timestamp <- x@timestamp
  autore <- rutils::whoami()
  helper <- x@helper
  df <- if (length(hash::keys(functions))) {
    DBI::dbGetQuery(con, sql_by_key(
      helper, "GET_CHANGED_FORMULE",
      tag = tag,
      last_updated = as.numeric(timestamp)))
  } else {
    data.frame()
  }

  names_with_conflicts <- intersect(x@touched, as.character(df$name))

  names_updated <- setdiff(hash::keys(x@functions), names_with_conflicts)
  if (length(names_updated)) {
    name <- NULL
    formule <- foreach::`%dopar%`(foreach::foreach(
      name = iterators::iter(names_updated), .combine = rbind), {
      formula <- expr(x, name, echo = FALSE)
      cbind(formula, autore, name, tag)
    })

    if (DBI::dbExistsTable(con, paste0("formule_", tag)) ||
         class(con) == "SQLiteConnection") {
      row <- NULL
      foreach::`%do%`(foreach::foreach(row = iterators::iter(formule, "row")), {
        formularow <- row[, 1]
        namerow <- row[, 3]

        DBI::dbExecute(con, sql_by_key(
          helper, "UPDATE_FORMULE",
          tag = tag,
          autore = autore,
          formula = formularow,
          name = namerow,
          msg = msg,
          last_updated = time_in_nano()))
      })
    }
    name <- NULL
    foreach::`%do%`(foreach::foreach(name = iterators::iter(names_updated)), {
      formula <- expr(x, name, echo = FALSE)
      DBI::dbExecute(con, sql_by_key(
        helper, "UPSERT_FORMULE",
        formula = formula,
        autore = autore,
        name = name,
        tag = tag,
        msg = msg,
        last_updated = time_in_nano()))
    })
  }

  if (interactive()) info("Update Functions done.", name = ln)
}
