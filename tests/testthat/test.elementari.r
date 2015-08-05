context("Serie Elementari")

test_that("posso avere serie con funzione senza padri", {
  g = GrafoDB("test")
  g["A"] <- TSERIES(c(1,2,3), START=c(1990,1), FREQ=4)
  g["B"] <- function() {
    B = TSERIES(c(1,2,3), START=c(1990,1), FREQ=4)
  }

  expect_true("B" %in% names(g))
  expect_equal(length(upgrf(g, "B")),0)
  expect_equal(g[["A"]], g[["B"]])
  expect_true(isElementary(g, "B"))
  expect_true(!isElementary(g, "A"))
})

elimina("test")
