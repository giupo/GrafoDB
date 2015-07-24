context("Funzioni per la persistenza")

g <- GrafoDB("test")
g["A"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
g["B"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
g["C"] <- function(A,B) {
  C = A + B    
}


test_that("Posso salvare e ricaricare da un file", {
  path <- tempfile()
  saveBinary(g, path)

  x <- readBinary(path)
  expect_true(is.grafodb(x))
  unlink(path)
})


