#' True if param is an error
#'
#' @name is_error
#' @usage is_error(cond)
#' @param cond argument to check
#' @return `TRUE` if cond is an error

is_error <- function(cond=NULL) {
  "simpleError" %in% class(cond)
}

#' this function returns the name of the missing object in
#' evaluation
#'
#' @name find_dep_from_error
#' @usage find_dep_from_error
#' @param cond error obtained in tryCatch
#' @seealso find_deps
#' @return single name of missing object blamed in `cond`, NA if not found

find_dep_from_error <- function(cond=NULL) {
  if (! is_error(cond)) {
    stop("`cond` is not an error: ", cond, ", class: ", class(cond))
  }
  match <- stringr::str_match(as.character(cond), "object '(\\w+)' not found")
  match[, 2]
}


#' finds and returns the deps of a formula as a character vector
#'
#' Given a formula for `C` like
#'    C <- A+B
#'    D <- C+B/2
#' the function returns `A` and `B` but not `C`
#'
#' @name find_deps
#' @usage find_deps(g, name, formula)
#' @param g anything that has names end data as
#'  list (env, list, dataset, GrafoDB)
#' @param formula the formula itself
#' @return list of names not defined in `formula` and needed to evaluate it
#' @export

find_deps <- function(g, formula) {
  env <- new.env()
  deps <- c()

  if (is.null(formula)) {
    return(NULL)
  }

  ret <- NULL
  while (is.null(ret) || is_error(ret)) {
    ret <- tryCatch({
      eval(parse(text = formula), envir = env)
    }, error = function(cond) {
      cond
    })

    if (is_error(ret)) {
      dep <- find_dep_from_error(ret)
      if (dep %in% names(g))  {
        env[[dep]] <- g[[dep]]
      } else {
        stop("I dunno where to look for: ", dep)
      }
      deps <- c(deps, dep)
    }
  }

  deps
}
