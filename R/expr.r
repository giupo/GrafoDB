#' Ritorna le formule del GrafoDB
#'
#' Ritorna (o mostra su console, stdout) le formule delle serie `nomi`
#'
#' @name  expr_impl
#' @usage expr_impl(x, name)
#' @usage expr_impl(x, name, echo=FALSE)
#' @param x istanza di GrafoDB
#' @param nomi array di nomi di serie storiche
#' @rdname expr-internal
#' @include functions.r

expr_impl <- function(x, nomi, echo=FALSE) {
  functions <- x@functions
  in_functions <- intersect(hash::keys(functions), nomi)
  to_be_loaded_from_db <- setdiff(nomi, in_functions)
  from_db <- if (length(to_be_loaded_from_db)) {
    dbformule <- x@dbformule
    dbformule[dbformule$name %in% nomi, c("name", "formula")]
  } else {
    data.frame(name = character(), formula = character())
  }

  in_functions <- foreach::`%do%`(foreach::foreach(
    row = iterators::iter(in_functions, by = "row"),
    .combine = rbind), {
      data.frame(name = row, formula = functions[[row]])
    })

  formule <- rbind(in_functions, from_db)

  if (nrow(formule) == 0) {
    NULL
  } else if (nrow(formule) == 1) {
    task <- as.character(formule$formula)
    if (interactive() && echo) {
      formatR::tidy_source(text = task, indent = 2) # nocov
    }
    task
  } else {
    nomi <- formule$name
    ret <- vector(length(nomi), mode = "list")
    for (i in seq_along(nomi)) {
      name <- nomi[[i]]
      ret[i] <- as.character(formule[formule$name == name, ]$formula)
    }
    names(ret) <- nomi
    ret
  }
}
