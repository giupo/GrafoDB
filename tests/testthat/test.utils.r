identicalts <- function(x, y, toll=0.000001) {
  all(abs(x - y) <= toll)
}

test_that("identicalts returns true if two arrays area equal", {
  expect_true(identicalts(c(0, 0, 0), c(0, 0, 0)))
})

test_that("identicalts returns false if two arrays area not equal", {
  expect_false(identicalts(c(0, 0, 0), c(0, 0, 1)))
})

test_that("identicalts returns false if two arrays area equal with a toll", {
  expect_true(identicalts(c(0, 0, 0), c(0, 0, 1), toll = 1))
})

test_that("identicalts yields false if two arrays area not equal with a toll", {
  expect_false(identicalts(c(0, 0, 0), c(0, 0, 0.1), toll = 0.01))
})