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

test_that("ts_differ returns false if series are equal (quarterly)", {
  x <- ts(c(1,2,3), start = c(1990,1), frequency = 4)
  y <- x

  expect_false(ts_differ(x, y))
})

test_that("ts_differ returns false if series are equal (monthly)", {
  x <- ts(c(1,2,3), start = c(1990,1), frequency = 12)
  y <- x

  expect_false(ts_differ(x, y))
})

test_that("ts_differ returns false if series are equal (yearly)", {
  x <- ts(c(1,2,3), start = c(1990,1), frequency = 1)
  y <- x

  expect_false(ts_differ(x, y))
})
test_that("ts_differ returns true if series differ (quarterly)", {
  x <- ts(c(1,2,3), start = c(1990,1), frequency = 4)
  y <- ts(c(1,2,4), start = c(1990,1), frequency = 4)

  expect_true(ts_differ(x, y))
})

test_that("ts_differ returns true if series differ (monthly)", {
  x <- ts(c(1,2,3), start = c(1990,1), frequency = 12)
  y <- ts(c(1,2,4), start = c(1990,1), frequency = 12)

  expect_true(ts_differ(x, y))
})

test_that("ts_differ returns true if series differ (yearly)", {
  x <- ts(c(1,2,3), start = c(1990,1), frequency = 1)
  y <- ts(c(1,2,4), start = c(1990,1), frequency = 1)

  expect_true(ts_differ(x, y))
})

test_that("ts_differ returns true if series differ (quarterly)", {
  x <- ts(c(1,2,3), start = c(1990,1), frequency = 4)
  y <- ts(c(1,2,4), start = c(1990,1), frequency = 4)

  expect_true(ts_differ(x, y))
})

test_that("ts_differ returns true if series differ (monthly)", {
  x <- ts(c(1,2,3), start = c(1990,1), frequency = 12)
  y <- ts(c(1,2,4), start = c(1990,1), frequency = 12)

  expect_true(ts_differ(x, y))
})

test_that("ts_differ returns true if series differ (yearly)", {
  x <- ts(c(1,2,3), start = c(1990,1), frequency = 1)
  y <- ts(c(1,2,4), start = c(1990,1), frequency = 1)

  expect_true(ts_differ(x, y))
})

test_that("ts_differ is false if series differ (quarterly) yet less than toll", {
  x <- ts(c(1,2,3), start = c(1990,1), frequency = 4)
  y <- ts(c(1,2,3.1), start = c(1990,1), frequency = 4)

  expect_false(ts_differ(x, y, 0.2))
})

test_that("ts_differ is false if series differ (monthly) yet less than toll", {
  x <- ts(c(1,2,3), start = c(1990,1), frequency = 12)
  y <- ts(c(1,2,3.1), start = c(1990,1), frequency = 12)

  expect_false(ts_differ(x, y, 0.2))
})

test_that("ts_differ is if series differ (yearly) yet less than toll", {
  x <- ts(c(1,2,3), start = c(1990,1), frequency = 1)
  y <- ts(c(1,2,3.1), start = c(1990,1), frequency = 1)

  expect_false(ts_differ(x, y, 0.2))
})

test_that("ts_differ returns true if series differ (quarterly)", {
  x <- ts(c(1,2,3), start = c(1990,1), frequency = 4)
  y <- ts(c(1,2,4), start = c(1990,1), frequency = 4)

  expect_true(ts_differ(x, y))
})

test_that("ts_differ returns true if series differ (monthly)", {
  x <- ts(c(1,2,3), start = c(1990,1), frequency = 12)
  y <- ts(c(1,2,4), start = c(1990,1), frequency = 12)

  expect_true(ts_differ(x, y))
})

test_that("ts_differ returns true if series differ (yearly)", {
  x <- ts(c(1,2,3), start = c(1990,1), frequency = 1)
  y <- ts(c(1,2,4), start = c(1990,1), frequency = 1)

  expect_true(ts_differ(x, y))
})

test_that("ts_differ returns true if index differ (quarterly)", {
  x <- ts(seq(1, 40), start = c(1990,1), frequency = 4)
  y <- ts(seq(1, 40), start = c(1991,1), frequency = 4)

  expect_true(ts_differ(x, y))
})

test_that("ts_differ returns true if index differ (monthly)", {
  x <- ts(seq(1, 40), start = c(1990,1), frequency = 12)
  y <- ts(seq(1, 40), start = c(1991,1), frequency = 12)

  expect_true(ts_differ(x, y))
})

test_that("ts_differ returns true if index differ (yearly)", {
  x <- ts(seq(1, 40), start = c(1990,1), frequency = 1)
  y <- ts(seq(1, 40), start = c(1991,1), frequency = 1)

  expect_true(ts_differ(x, y))
})

test_that("ts_differ returns true if index differ in length (quarterly)", {
  x <- ts(seq(1, 40), start = c(1990,1), frequency = 4)
  y <- ts(seq(1, 41), start = c(1990,1), frequency = 4)

  expect_true(ts_differ(x, y))
})

test_that("ts_differ returns true if index differ in length (monthly)", {
  x <- ts(seq(1, 40), start = c(1990,1), frequency = 12)
  y <- ts(seq(1, 41), start = c(1990,1), frequency = 12)

  expect_true(ts_differ(x, y))
})

test_that("ts_differ returns true if index differ in length (yearly)", {
  x <- ts(seq(1, 40), start = c(1990,1), frequency = 1)
  y <- ts(seq(1, 41), start = c(1990,1), frequency = 1)

  expect_true(ts_differ(x, y))
})


test_that("ts_differ if not intersecting (quarterly)", {
  x <- ts(c(1,2,3), start = c(1990,1), frequency = 4)
  y <- ts(c(1,2,3), start = c(1991,1), frequency = 4)

  expect_warning(expect_true(ts_differ(x, y)))
})

test_that("ts_differ returns true if index differ in length (monthly)", {
  x <- ts(c(1, 2, 3), start = c(1990,1), frequency = 12)
  y <- ts(c(1, 2, 3), start = c(1991,1), frequency = 12)

  expect_warning(expect_true(ts_differ(x, y)))
})

test_that("ts_differ returns true if index differ in length (yearly)", {
  x <- ts(c(1, 2, 3), start = c(1990,1), frequency = 1)
  y <- ts(c(1, 2, 3), start = c(2000,1), frequency = 1)

  expect_warning(expect_true(ts_differ(x, y)))
})


