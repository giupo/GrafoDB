setup <- function(tag) {
  g <- GrafoDB(tag)
  g["A"] <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  g["B"] <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C = A + B # nolint
  }
  g
}

test_that("I get no diff on the same Graph", {
  g <- setup("test")
  on.exit(delete_graph("test"))

  expect_equal(nrow(diff.GrafoDB(g, g)), 0)
})

test_that("I get a warning for uncommon names between graphs", {
  g <- setup("test")
  g1 <- setup("test2")

  on.exit({
    delete_graph("test")
    delete_graph("test2")
  })

  g1["D"] <- function(A) { # nolint
    D = A * 2 # nolint
  }
  expect_warning(diff.GrafoDB(g, g1))
  expect_warning(expect_equal(nrow(diff.GrafoDB(g, g1)), 0))
})

test_that("I get a dataframe with the difference in formulas", {
  g <- setup("test")
  g1 <- setup("test2")

  on.exit({
    delete_graph("test")
    delete_graph("test2")
  })
  g <- saveGraph(g)
  g1 <- saveGraph(g1)

  g1["C"] <- function(A, B) { # nolint
    C = A - B # nolint
  }

  g1 <- saveGraph(g1)
  expect_equal(nrow(diff.GrafoDB(g, g1)), 1)
})

test_that("I get authors in each data.frame", {
  g <- setup("test")
  g1 <- setup("test2")

  on.exit({
    delete_graph("test")
    delete_graph("test2")
  })

  g <- saveGraph(g)
  g1 <- saveGraph(g1)

  g1["C"] <- function(A, B) { # nolint
    C = A - B # nolint
  }
  g1 <- saveGraph(g1)

  dd <- diff.GrafoDB(g, g1)
  expect_true("test_autore" %in% colnames(dd))
  expect_true("test2_autore" %in% colnames(dd))
})
