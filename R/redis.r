redisMakeKey <- function(name, tag, what) {
  paste(name, tag, what, sep="|")
}

#' @importFrom rredis redisConnect redisDelete redisClose

removeFromRedis <- function(x, nomi) {
  tag <- x@tag
  settings <- dbSettings()
  tryCatch({
    redisConnect(
      host = settings$Redis$host,
      port = as.integer(settings$Redis$port))
    
    for(name in nomi) {
      key <- redisMakeKey(name, tag, 'data')
      suppressWarnings(redisDelete(key))
    }
    redisClose()
  })
}

#' @importFrom rredis redisConnect redisDelete redisClose redisLPush

sendCopyMetadati <- function(sourceTag, destTag) {
  msg = paste(sourceTag, destTag, sep="|")
  settings <- dbSettings()
  tryCatch({
    redisConnect(host = settings$Redis$host,
                 port = as.integer(settings$Redis$port))
    redisLPush('grafo-copymetadati', charToRaw(msg))
    redisClose()
  })
}

