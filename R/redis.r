redisMakeKey <- function(name, tag, what) {
  paste0(name, tag, what, sep="|")
}

#' @importFrom rredis redisConnect redisDelete

removeFromRedis <- function(x, nomi) {
  tag <- x@tag
  settings <- dbSettings()
  tryCatch({
    redisConnect(host = settings$redisHost, port = as.integer(settings$redisPort))
    for(name in nomi) {
      key <- redisMakeKey(name, tag, 'data')
      suppressWarnings(redisDelete(key))
    }
    redisClose()
  })
}
