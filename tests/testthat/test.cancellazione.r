context("Deleting objects")

setup <- function(tag) {
  options(SQLHelperType="SQLite")
  g <- GrafoDB(tag)
  g["A"] <- g["B"] <- ts(c(0,0,0), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) {
    C = A + B
  }
  g
}

test_that("Posso cancellare serie", {
  g <- setup("test")
  expect_error(rmNode(g, "A"))
  g <- rmNode(g, "C")
  expect_true(!"C" %in% names(g))
  delete_graph("test")
})


test_that("Posso cancellare serie ricorsivamente", {
  g <- setup("test")
  g <- rmNode(g, "A", TRUE)
  expect_true(!"A" %in% names(g))
  expect_true(!"C" %in% names(g))
  expect_true("B" %in% names(g))
  delete_graph("test")
})

### --- persistenza

test_that("Posso cancellare serie, DB", {
  g <- setup("test")
  g <- saveGraph(g)
  expect_error(rmNode(g, "A"))
  g <- rmNode(g, "C")
  expect_true(!"C" %in% names(g))
  delete_graph("test")
})

test_that("Posso cancellare serie ricorsivamente, DB", {
  g <- setup("test")
  g <- saveGraph(g)
  g <- rmNode(g, "A", TRUE)
  expect_true(!"A" %in% names(g))
  expect_true(!"C" %in% names(g))
  expect_true("B" %in% names(g))
  delete_graph("test")
})

