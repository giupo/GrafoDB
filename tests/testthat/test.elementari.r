context("Serie Elementari")

setup <- function() {
  g <- GrafoDB("test")
  g["A"] <- ts(c(1,2,3), start=c(1990,1), freq=4)
  g["B"] <- function() {
    B = ts(c(1,2,3), start=c(1990,1), freq=4)
  }
  g
}

test_that("posso avere serie con funzione senza padri", {
  g <- setup()
  expect_true("B" %in% names(g))
  expect_equal(length(upgrf(g, "B")), 0)
  expect_true(all(g[["A"]] == g[["B"]]))
  expect_true(isElementary(g, "B"))
  expect_true(!isElementary(g, "A"))
  elimina(g)
})


test_that("posso usare un elementare a patto che esistano i dati nel db",{
  g <- setup()
  g@data[["C"]] <- ts(c(1,2,3), start = c(1990, 1), freq = 4)
  g["C"] <- function() {
    C[3] <- 4
  }
  # expect_equal(length(g) , 3)
  expect_equal(g[["C"]][3], 4)
  expect_error({
    g["NONESISTO"] <- function() {
      NONESISTO[1] <- 4
    }
  }, "Error in NONESISTO")
  elimina(g)
})
