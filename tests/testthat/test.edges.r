context("Edges")

setup <- function(tag) {
  g <- GrafoDB(tag)
  g["A"] <- ts(runif(10), start = c(1990, 1), frequency = 1)
  g["B"] <- ts(runif(10), start = c(1990, 1), frequency = 1)
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }
  g
}


test_that("I get a warning if deps are more than needed", {
  g <- setup("test")
  on.exit(delete_graph(g))
  expect_warning(g["D"] <- function(A, B, C) { # nolint
    D <- C * 2 # nolint
  })
})

test_that("I get to remove arcs", {
  g <- setup("test")
  on.exit(delete_graph("test"))

  g <- saveGraph(g)

  g["C"] <- function(A) { # nolint
    C <- A # nolint
  }

  g <- saveGraph(g)

  expect_true(!"B" %in% upgrf(g, "C", livello = 1))
})
