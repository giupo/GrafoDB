context("metadati")

g <- GrafoDB("test")
g["A"] <- g["B"] <- TSERIES(c(0,0,0), START=c(1990,1), FREQ=1)
setMeta(g, "A", "KEY", "VALUE1")
setMeta(g, "A", "KEY", "VALUE2")
setMeta(g, "B", "KEY", "VALUE1")

test_that("posso ottenere i metadati per una serie", {
  m <- getMeta(g, "A")
  expect_true(is.data.frame(m))
  expect_equal(nrow(m), 2)
})

test_that("posso ottenere i valori di un metadato per singola serie", {
  v <- getMeta(g, "A", "KEY")
  expect_true(is.character(v))
  expect_true(all(c("VALUE1", "VALUE2") %in% v ))
})

elimina("test")
