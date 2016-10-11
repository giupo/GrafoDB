#' @importFrom RCurl getURL
#' @importFrom RJSONIO fromJSON

get_tickets_urls_for_name <- function(g, name) {
  settings <- dbSettings()
  stopifnot(name %in% names(g))
  base_url <- paste0(settings$WebApp$base_url, "/grafo/tickets/search?term=")
  url <- paste0(base_url, name)
  res <- getURL(url, .opts=list(ssl.verifypeer=F))
  lista <- fromJSON(res)
  unlist(lapply(lista, function(x) x[["url"]])) # estraggo solo l'url
}

