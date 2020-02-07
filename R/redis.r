redisMakeKey <- function(name, tag, what) {
  paste(name, tag, what, sep="|")
}

removeFromRedis <- function(x, nomi) {
  tag <- x@tag
  settings <- dbSettings()
  tryCatch({
    suppressWarnings(rredis::redisConnect(
      host = settings$Redis$host,
      port = as.integer(settings$Redis$port)))
    
    for(name in nomi) {
      key <- redisMakeKey(name, tag, 'data')
      suppressWarnings(rredis::redisDelete(key))
    }
    rredis::redisClose()
  }, error=function(cond) {
  })
}

sendCopyMetadati <- function(sourceTag, destTag) {
  msg = paste(sourceTag, destTag, sep="|")
  settings <- dbSettings()
  tryCatch({
      suppressWarnings(rredis::redisConnect(host = settings$Redis$host,
                 port = as.integer(settings$Redis$port)))
    rredis::redisLPush('grafo-copymetadati', charToRaw(msg))
    rredis::redisClose()
  }, error=function(cond) {     
  })     
}
