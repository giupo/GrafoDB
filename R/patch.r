#' apply a patch (from a diff)
#'
#' a diff on GrafoDB are differences in formulas between two editions of a
#' GrafoDB.
#'
#' @name patch
#' @seealso diff.GrafoDB
#' @param x GrafoDB instance
#' @param diff_ differences (evaluated with diff.GrafoDB)
#' @param column which column of diff_ to apply on Grafo
#' @note Experimental
#' @export
#' @include logging.r


patch <- function(x, diff_, column=NULL) {
  if (!is.null(column)) {
    patch_by_column(x, diff_, column)
  } else {
    patch_by_lastupdated(x, diff_)
  }
  x
}


patch_by_lastupdated <- function(x, diff_) {
  ln <- "GrafoDB.patch.patch_by_lastupdated"
  nomi <- diff_$name
  for (nome in nomi) {
    trace("Patching %s", nome, name = ln)
    lastup1 <- as.numeric(diff_[diff_$name == nome, 4])
    lastup2 <- as.numeric(diff_[diff_$name == nome, 7])
    new_formula <- if (lastup1 > lastup2) {
      trace("nuova data (%s) > vecchia data (%s)", lastup1, lastup2, name = ln)
      as.character(diff_[diff_$name == nome, 2])
    } else if (lastup1 < lastup2) {
      trace("nuova data (%s) < vecchia data (%s)", lastup1, lastup2, name = ln)
      as.character(diff_[diff_$name == nome, 5])
    } else {
      stop("don't know what to pick for ", nome,
           " while patching. Check your diff last_updated fields")
    }

    trace("New formula patch: %s", new_formula, name = ln)
    x@functions[[nome]] <- new_formula
  }

  evaluate(x, nomi)
  x
}


patch_by_column <- function(x, diff_, column=NULL) {
  ln <- "GrafoDB.patch.patch_by_column"
  nomi <-  diff_$name
  for (nome in nomi) {
    trace("Patching %s", nome, name = ln)
    new_formula <- as.character(diff_[diff_$name == nome, column])
    trace("New formula patch: %s", new_formula, name = ln)
    x@functions[[nome]] <- new_formula
  }
  evaluate(x, nomi)
  x
}
