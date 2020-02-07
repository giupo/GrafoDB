#' Ritorna le formule del GrafoDB
#'
#' Ritorna (o mostra su console, stdout) le formule delle serie `nomi`
#'
#' @name  .expr
#' @usage .expr(x, name)
#' @usage .expr(x, name, echo=FALSE)
#' @param x istanza di GrafoDB
#' @param nomi array di nomi di serie storiche
#' @rdname expr-internal
#' @importFrom formatR tidy_source
#' @include functions.r

.expr <- function(x, nomi, echo=FALSE) {
  functions <- x@functions
  in.functions <- intersect(hash::keys(functions), nomi)
  da.caricare.db <- setdiff(nomi, in.functions)
  from.db <- if(length(da.caricare.db)) {
    dbformule <- x@dbformule
    dbformule[dbformule$name %in% nomi, c("name", "formula")]
  } else {
    data.frame(name=character(), formula=character())
  }
   
  in.functions <- foreach(
    row=iter(in.functions, by='row'),
    .combine=rbind) %do% {
      data.frame(name=row, formula=functions[[row]])
    }
  
  formule <- rbind(in.functions, from.db)
  
  if(nrow(formule) == 0) {
    NULL
  } else if(nrow(formule) == 1) {
    task <- as.character(formule$formula)
    if(interactive() && echo) {
      tidy_source(text=task, indent = 2) # nocov
    }
    task
  } else {
    nomi <- formule$name
    ret <- vector(length(nomi), mode = "list")
    for(i in seq_along(nomi)) {
      name <- nomi[[i]]
      ret[i] <- as.character(formule[formule$name == name,]$formula)
    }
    names(ret) <- nomi
    ret
  }
}
