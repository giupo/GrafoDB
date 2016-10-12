#' @importFrom RCurl getURL
#' @importFrom RJSONIO fromJSON

get_tickets_urls_for_name <- function(g, name) {
  settings <- dbSettings()
  stopifnot(name %in% names(g))
  base_url <- paste0(settings$WebApp$base_url, "/tickets/search?term=")
  url <- paste0(base_url, name)
  res <- getURL(url, .opts=list(ssl.verifypeer=F))
  lista <- fromJSON(res)
  unlist(lapply(lista, function(x) x[["url"]])) # estraggo solo l'url
}


#' ritorna i tickets aperti nel track
#'
#' @name ticket
#' @usage ticket(num)
#' @param num id del ticket
#' @return una rappresentazione a ticket (alla meglio di R)
#' @export

ticket <- function(num) {
  settings <- dbSettings()
  base_url <- paste0(settings$WebApp$base_url, "/tickets/get")
  url <- paste0(base_url, "/", num)
  res <- getURL(url, .opts=list(ssl.verifypeer=F))
  fromJSON(res)
}

