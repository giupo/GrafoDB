context("Serie Elementari")

g <- GrafoDB("test")
g["A"] <- TSERIES(c(1,2,3), START=c(1990,1), FREQ=4)
g["B"] <- function() {
  B = TSERIES(c(1,2,3), START=c(1990,1), FREQ=4)
}

test_that("posso avere serie con funzione senza padri", {
  expect_true("B" %in% names(g))
  expect_equal(length(upgrf(g, "B")),0)
  expect_equal(g[["A"]], g[["B"]])
  expect_true(isElementary(g, "B"))
  expect_true(!isElementary(g, "A"))
})

test_that("posso usare un elementare a patto che esistano i dati nel db",{
  g@data[["C"]] <- TSERIES(c(1,2,3), START=c(1990,1), FREQ=4)
  g["C"] <- function() {
    C[[1990,4]] <- 4
  }
  # expect_equal(length(g) , 3)
  expect_equal(g[["C"]][[1990,4]], 4)
  expect_error({
    g["NONESISTO"] <- function() {
      NONESISTO[[1990,4]] <- 4
    }
  }, "Error in NONESISTO")
})

elimina("test")
