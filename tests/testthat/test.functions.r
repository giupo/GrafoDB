context("Util functions")

test_that("to.data.frame converte correttamente una serie", {
  
  tt <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  df <- to.data.frame(tt, "MAMMA CIUCCIO")
  expect_true(identical(tt, from.data.frame(df)[[df$name]]))
})
