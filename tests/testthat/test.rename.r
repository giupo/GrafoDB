context("Rename nodes")

test_that("I can rename a series before saving it", {
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C = A + B # nolint
  }
  g["D"] <- function(A, C) { # nolint
    D = A + C # nolint
  }
  g <- saveGraph(g)
  g <- rename(g, "A", "A1")

  expect_true(!"A" %in% names(g))
  expect_true("A1" %in% names(g))
  expect_true(grepl("A1", expr(g, "C")))
  expect_true(grepl("A1", expr(g, "D")))
  expect_true("A1" %in% upgrf(g, "D", livello = 1))
  expect_true("A1" %in% upgrf(g, "C", livello = 1))
})

delete_graph("test")

test_that("I cannot rename a series being modified", {
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  expect_error(rename(g, "A", "A1"))
  g["C"] <- function(A, B) { # nolint
    C = A + B # nolint
  }
  expect_error(rename(g, "A", "A1"))
  g <- saveGraph(g)
  g <- rename(g, "A", "A1")
  expect_true(!"A" %in% names(g))
  expect_true("A1" %in% names(g))
})

delete_graph("test")

test_that("I cannot rename a series into an existing series", {
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C = A + B # nolint
  }
  expect_error(rename(g, "A", "B"))
  g <- saveGraph(g)
  expect_error(rename(g, "A", "B"))
})

delete_graph("test")

test_that("I cannot rename a non existing series", {
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C = A + B # nolint
  }
  expect_error(rename(g, "D", "D1"))
  g <- saveGraph(g)
  expect_error(rename(g, "D", "D1"))
})

delete_graph("test")
