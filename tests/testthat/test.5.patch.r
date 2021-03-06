setup <- function(x) {
  g <- GrafoDB(x)
  g["A"] <- g["B"] <- stats::ts(rep(1, 10), start = c(1990, 1), frequency = 1)
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }
  g
}

test_that("if I apply a patch I get a new GrafoDB with new formulas", {
  g <- setup("test")
  on.exit({
    delete_graph("test")
    delete_graph("primo")
    delete_graph("secondo")
  })

  saveGraph(g)

  g1 <- GrafoDB("test")

  g1["C"] <- function(A, B) { # nolint
    C <- A - B # nolint
  }

  g1 <- saveGraph(g1, "primo")

  g2 <- GrafoDB("test")
  g2["C"] <- function(A, B) { # nolint
    C <- A * B # nolint
  }

  g2 <- saveGraph(g2, "secondo")

  diffe <- diff(g1, g2)
  expect_equal(nrow(diffe), 1)

  x <- patch(g2, diffe, "primo")
  expect_equal(expr(x, "C"), expr(g2, "C"))

})

test_that("If no column from diff is specified, it gets the most recent", {
  g <- setup("test")
  on.exit({
    delete_graph("test")
    delete_graph("primo")
    delete_graph("secondo")
  })

  saveGraph(g)

  g1 <- GrafoDB("test")
  g1["C"] <- function(A, B) { # nolint
    C <- A - B # nolint
  }
  g1 <- saveGraph(g1, "primo")

  g2 <- GrafoDB("test")
  g2["C"] <- function(A, B) { # nolint
    C <- A * B # nolint
  }

  g2 <- saveGraph(g2, "secondo")


  diffe <- diff(g1, g2)

  x <- patch(g1, diffe)
  expect_equal(expr(x, "C"), expr(g2, "C"))
})
