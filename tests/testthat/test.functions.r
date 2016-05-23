context("Util functions")

test_that("to.data.frame converte correttamente una serie", {  
  tt <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  df <- to.data.frame(tt, "TEST")
  expect_true(identical(tt, from.data.frame(df)[[df$name]]))
})

test_that("to.data.frame converte correttamente vettori", {  
  tt <- runif(10)
  df <- to.data.frame(tt, "TEST")
  expect_true(identical(tt, from.data.frame(df)$TEST))
})

test_that("to.data.frame converte correttamente scalari", {  
  tt <- 1
  df <- to.data.frame(tt, "TEST")
  expect_true(identical(tt, from.data.frame(df)$TEST))
})


test_that("to.data.frame converte correttamente missing", {  
  tt <- NA
  df <- to.data.frame(tt, "TEST")
  expect_true(identical(tt, from.data.frame(df)$TEST))
})

test_that("to.data.frame converte correttamente vettori di missing", {  
  tt <- c(NA, NA)
  df <- to.data.frame(tt, "TEST")
  expect_true(identical(tt, from.data.frame(df)$TEST))
})

test_that(".declutter_functions removes correctly functions from it's definitions", {
  f <- "function(A,B,C) { A = 1 }"
  f <- .declutter_function(f)
  expect_equal(f, "A = 1")

  f <- function(A,B,C) {
    A = A+B+C
  }

  f <- .declutter_function(f)
  expect_equal(f, "A = A+B+C")
})

test_that(".decluter_functions preserves commnets", {
  f <- function(A) {
    # comment here
    A
  }

  f <- .declutter_function(f)
  expect_equal(f, "# comment here\n    A")

  f <- "function(A) { # comment here\nA}"
  f <- .declutter_function(f)
  expect_equal(f, "# comment here\nA")
})
