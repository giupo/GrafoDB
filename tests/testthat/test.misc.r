context("Misc functions")

test_that("conversione da bimets a data.frame", {
  tt <- TSERIES(runif(10), START=c(1990,1), FREQ=1)
  df <- to.data.frame(tt, "test")
  expect_true(is.data.frame(df))
  expect_true(all(c("name", "anno", "periodo", "freq", "dati") %in% names(df)))

  tt <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  df <- to.data.frame(tt, "test")
  expect_true(is.data.frame(df))
  expect_true(all(c("name", "anno", "periodo", "freq", "dati") %in% names(df)))


  tt <- TSERIES(runif(10), START=c(1990,1), FREQ=12)
  df <- to.data.frame(tt, "test")
  expect_true(is.data.frame(df))
  expect_true(all(c("name", "anno", "periodo", "freq", "dati") %in% names(df)))
})

test_that("conversione da data.frame a bimets", {
  df <- as.data.frame(list(name="TEST", anno=1990, periodo=1, freq=1, dati="[1,2,3,4,5]"), stringsAsFactors = F)
  tt <- from.data.frame(df)
  expect_true(is.list(tt))

  tt <- tt[["TEST"]]
  expect_true(is.bimets(tt))
  expect_equal(TSINFO(tt, MODE="STARTY"), 1990)
  expect_equal(TSINFO(tt, MODE="STARTP"), 1)
  expect_equal(TSINFO(tt, MODE="FREQ"), 1)

  df <- as.data.frame(list(name="TEST", anno=1990, periodo=1, freq=12, dati="[1,2,3,4,5]"), stringsAsFactors = F)
  tt <- from.data.frame(df)
  

  expect_true(is.list(tt))
  tt <- tt[["TEST"]]
  expect_true(is.bimets(tt))
  expect_equal(TSINFO(tt, MODE="STARTY"), 1990)
  expect_equal(TSINFO(tt, MODE="STARTP"), 1)
  expect_equal(TSINFO(tt, MODE="FREQ"), 12)

  df <- as.data.frame(list(name="TEST", anno=1990, periodo=1, freq=4, dati="[1,2,3,4,5]"), stringsAsFactors = F)
  tt <- from.data.frame(df)
  expect_true(is.list(tt))
  tt <- tt[["TEST"]]
  expect_true(is.bimets(tt))
  expect_equal(TSINFO(tt, MODE="STARTY"), 1990)
  expect_equal(TSINFO(tt, MODE="STARTP"), 1)
  expect_equal(TSINFO(tt, MODE="FREQ"), 4)
})

test_that("valori null passati a from.data.frame vengono convertiti in NA", {
  df <- as.data.frame(list(name="TEST", anno=1990, periodo=1, freq=4, dati="[null, 1,2,3,4,null]"), stringsAsFactors = F)
  tt <- from.data.frame(df)
  expect_true(is.list(tt))
  tt <- tt[["TEST"]]
  expect_true(is.bimets(tt))
  expect_true(is.na(last(tt)))
  expect_true(is.na(first(tt)))
})
