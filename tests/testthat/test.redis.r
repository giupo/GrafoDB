context('Redis')


test_that("redisMakeKey works as expected", {
  expect_equal(redisMakeKey("A", "B", "C"), "A|B|C")
})

test_that("RemoveFromRedis works as expected", {
  on.exit(elimina("test"))
  g <- GrafoDB("test")
  with_mock(
    'rredis::redisConnect' = function(...) {},
    'rredis::redisDelete' = function(b) {
      expect_equal(redisMakeKey("A", "test", "data"))
    },
    'rredis::redisClose' = function(...) {}, {
      removeFromRedis(g, "A")
    })
})

test_that("RemoveFromRedis is silent", {
  on.exit(elimina("test"))
  g <- GrafoDB("test")
  with_mock(
    'rredis::redisConnect' = function(...) {
      warning("Should never be seen")
    },
    'rredis::redisDelete' = function(b) {
      warning("Should never be seen")
    },
    'rredis::redisClose' = function(...) {}, {
      expect_silent(removeFromRedis(g, "A"))
    })
})

test_that("RemoveFromRedis is silent", {
  with_mock(
    'rredis::redisConnect' = function(...) {},
    'rredis::redisLPush' = function(...) {
      warning("I got called")
    },
    'rredis::redisClose' = function(...) {}, {
      expect_warning(sendCopyMetadati("A", "B"), "I got called")
    })
})
