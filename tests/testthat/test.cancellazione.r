context("cancellazione serie")

test_that("Posso cancellare serie", {
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- TSERIES(c(0,0,0), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B
  }
  
  expect_error(rmNode(g, "A"))
  g <- rmNode(g, "C")
  expect_true(!"C" %in% names(g))
  elimina("test")
})

test_that("Posso cancellare serie ricorsivamente", {
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- TSERIES(c(0,0,0), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B
  }
  g <- rmNode(g, "A", TRUE)
  expect_true(!"A" %in% names(g))
  expect_true(!"C" %in% names(g))
  expect_true("B" %in% names(g))
  elimina("test")
})


### --- persistenza

test_that("Posso cancellare serie, DB", {
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- TSERIES(c(0,0,0), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B
  }
  g <- saveGraph(g)
  expect_error(rmNode(g, "A"))
  g <- rmNode(g, "C")
  expect_true(!"C" %in% names(g))
  elimina("test")
})

test_that("Posso cancellare serie ricorsivamente, DB", {
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- TSERIES(c(0,0,0), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B
  }
  g <- saveGraph(g)
  g <- rmNode(g, "A", TRUE)
  expect_true(!"A" %in% names(g))
  expect_true(!"C" %in% names(g))
  expect_true("B" %in% names(g))
  elimina("test")
})

