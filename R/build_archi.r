#' this function extract deps based on pattern matching on nodes


rebuild_edges <- function(x) {
  nomi <- names(g)
  archi <- data.frame(partenza = c(), arrivo = c())
  total <- length(nomi)
  pb <- progress::progress_bar$new(
    format = ":what [:bar] :percent eta: :eta", total = total)

  for (name in nomi) {
    pb$tick(tokens = list(what = name))
    formula <- expr(g, name)
    if (is.null(formula)) next

    per_righe <- unlist(stringr::str_split(formula, "\\n"))
    # tolgo gli spazi
    per_righe <- stringr::str_trim(per_righe)
    # cerco di togliere i commenti.
    per_righe <- per_righe[!stringr::str_starts(per_righe, "#")]

    formula <- paste(per_righe, collapse = "\\n")

    deps <- unlist(stringr::str_extract_all(formula, "\\w+"))
    deps <- intersect(deps, nomi)
    deps <- setdiff(deps, name)
    if (length(deps) == 0) next

    nuovi_archi <- data.frame(
      partenza = deps,
      arrivo = rep(name, length(deps)))

    archi <- rbind(archi, nuovi_archi)
  }
  archi
}