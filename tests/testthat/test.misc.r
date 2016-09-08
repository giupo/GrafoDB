context("Misc functions")

test_that("conversione da ts a data.frame", {
  tt <- ts(runif(10), start=c(1990,1), freq=1)
  df <- to.data.frame(tt, "test")
  expect_true(is.data.frame(df))
  expect_true(all(c("name", "anno", "periodo", "freq", "dati") %in% names(df)))

  tt <- ts(runif(10), start=c(1990,1), freq=4)
  df <- to.data.frame(tt, "test")
  expect_true(is.data.frame(df))
  expect_true(all(c("name", "anno", "periodo", "freq", "dati") %in% names(df)))


  tt <- ts(runif(10), start=c(1990,1), freq=12)
  df <- to.data.frame(tt, "test")
  expect_true(is.data.frame(df))
  expect_true(all(c("name", "anno", "periodo", "freq", "dati") %in% names(df)))
})

test_that("conversione da data.frame a bimets", {
  df <- as.data.frame(list(name="TEST", anno=1990, periodo=1, freq=1, dati="[1,2,3,4,5]"), stringsAsFactors = F)
  tt <- from.data.frame(df)
  expect_true(is.list(tt))

  tt <- tt[["TEST"]]
  expect_true(is.ts(tt))
  expect_equal(start(tt)[[1]], 1990)
  expect_equal(start(tt)[[2]], 1)
  expect_equal(frequency(tt), 1)

  df <- as.data.frame(list(name="TEST", anno=1990, periodo=1, freq=12, dati="[1,2,3,4,5]"), stringsAsFactors = F)
  tt <- from.data.frame(df)
  

  expect_true(is.list(tt))
  tt <- tt[["TEST"]]
  expect_true(is.ts(tt))
  expect_equal(start(tt)[[1]], 1990)
  expect_equal(start(tt)[[2]], 1)
  expect_equal(frequency(tt), 12)

  df <- as.data.frame(list(name="TEST", anno=1990, periodo=1, freq=4, dati="[1,2,3,4,5]"), stringsAsFactors = F)
  tt <- from.data.frame(df)
  expect_true(is.list(tt))
  tt <- tt[["TEST"]]
  expect_true(is.ts(tt))
  expect_equal(start(tt)[[1]], 1990)
  expect_equal(start(tt)[[2]], 1)
  expect_equal(frequency(tt), 4)
})

test_that("valori null passati a from.data.frame vengono convertiti in NA", {
  df <- as.data.frame(list(name="TEST", anno=1990, periodo=1, freq=4, dati="[null, 1,2,3,4,null]"), stringsAsFactors = F)
  tt <- from.data.frame(df)
  expect_true(is.list(tt))
  tt <- tt[["TEST"]]
  expect_true(is.ts(tt))
  expect_true(is.na(last(tt)))
  expect_true(is.na(first(tt)))
})


test_that("tsdiff returns FALSE if two timeseries are equal",  {
  a <- b <- ts(runif(10), start=c(1990,1), freq=1)
  expect_true(!tsdiff(a,b))
  a <- b <- ts(runif(10), start=c(1990,1), freq=4)
  expect_true(!tsdiff(a,b))
  a <- b <- ts(runif(10), start=c(1990,1), freq=12)
  expect_true(!tsdiff(a,b))
})

test_that("tsdiff returns TRUE if two timeseries differ",  {
  a <- ts(runif(10), start=c(1990,1), freq=1)
  b <- ts(runif(10), start=c(1990,1), freq=1)
  expect_true(tsdiff(a,b))
  a <- ts(runif(10), start=c(1990,1), freq=4)
  b <- ts(runif(10), start=c(1990,1), freq=4)
  expect_true(tsdiff(a,b))
  a <- ts(runif(10), start=c(1990,1), freq=12)
  b <- ts(runif(10), start=c(1990,1), freq=12)
  expect_true(tsdiff(a,b))
})

test_that("tsdiff returns TRUE if two timeseries have different index",  {
  a <- ts(runif(10), start=c(1990,1), freq=1)
  b <- ts(runif(10), start=c(1990,1), freq=12)
  expect_true(tsdiff(a,b))
  a <- ts(runif(10), start=c(1990,1), freq=4)
  b <- ts(runif(10), start=c(1990,1), freq=12)
  expect_true(tsdiff(a,b))
  a <- ts(runif(10), start=c(1990,1), freq=1)
  b <- ts(runif(10), start=c(1990,1), freq=4)
  expect_true(tsdiff(a,b))
})

test_that("tsdiff returns TRUE if two timeseries have different dimensions",  {
  v1 <- c(1,2,3,4,5)
  v2 <- c(v1, 6)
  a <- ts(v1, start=c(1990,1), freq=1)
  b <- ts(v2, start=c(1990,1), freq=1)
  expect_true(tsdiff(a,b))
  a <- ts(v1, start=c(1990,1), freq=4)
  b <- ts(v2, start=c(1990,1), freq=4)
  expect_true(tsdiff(a,b))
  a <- ts(v1, start=c(1990,1), freq=12)
  b <- ts(v2, start=c(1990,1), freq=12)
  expect_true(tsdiff(a,b))
})
