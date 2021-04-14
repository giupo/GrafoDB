context("Miscellaneous functions")

test_that("conversione da ts a data.frame", {
  tt <- ts(runif(10), start = c(1990, 1), frequency = 1)
  df <-  to_data_frame(tt, "test")
  expect_true(is.data.frame(df))
  expect_true(all(c("name", "anno", "periodo", "freq", "dati") %in% names(df)))

  tt <- ts(runif(10), start = c(1990, 1), frequency = 4)
  df <-  to_data_frame(tt, "test")
  expect_true(is.data.frame(df))
  expect_true(all(c("name", "anno", "periodo", "freq", "dati") %in% names(df)))


  tt <- ts(runif(10), start = c(1990, 1), frequency = 12)
  df <-  to_data_frame(tt, "test")
  expect_true(is.data.frame(df))
  expect_true(all(c("name", "anno", "periodo", "freq", "dati") %in% names(df)))
})

test_that("conversione da data.frame a bimets", {
  df <- as.data.frame(
    list(name = "TEST",
      anno = 1990,
      periodo = 1,
      freq = 1,
      dati = "[1,2,3,4,5]"),
    stringsAsFactors = FALSE)

  tt <- from_data_frame(df)
  expect_true(is.list(tt))

  tt <- tt[["TEST"]]
  expect_true(is.ts(tt))
  expect_equal(stats::start(tt)[[1]], 1990)
  expect_equal(stats::start(tt)[[2]], 1)
  expect_equal(stats::frequency(tt), 1)

  df <- as.data.frame(
    list(name = "TEST",
      anno = 1990,
      periodo = 1,
      freq = 12,
      dati = "[1,2,3,4,5]"),
    stringsAsFactors = FALSE)

  tt <- from_data_frame(df)

  expect_true(is.list(tt))
  tt <- tt[["TEST"]]
  expect_true(is.ts(tt))
  expect_equal(stats::start(tt)[[1]], 1990)
  expect_equal(stats::start(tt)[[2]], 1)
  expect_equal(stats::frequency(tt), 12)

  df <- as.data.frame(
    list(name = "TEST",
      anno = 1990,
      periodo = 1,
      freq = 4,
      dati = "[1,2,3,4,5]"),
    stringsAsFactors = FALSE)

  tt <- from_data_frame(df)
  expect_true(is.list(tt))
  tt <- tt[["TEST"]]
  expect_true(is.ts(tt))
  expect_equal(stats::start(tt)[[1]], 1990)
  expect_equal(stats::start(tt)[[2]], 1)
  expect_equal(stats::frequency(tt), 4)
})

test_that("valori null passati a from_data_frame vengono convertiti in NA", {
  df <- as.data.frame(
    list(name = "TEST",
      anno = 1990,
      periodo = 1,
      freq = 4,
      dati = "[null, 1,2,3,4,null]"),
    stringsAsFactors = FALSE)

  tt <- from_data_frame(df)
  expect_true(is.list(tt))
  tt <- tt[["TEST"]]
  expect_true(stats::is.ts(tt))
  expect_true(is.na(xts::last(tt)))
  expect_true(is.na(xts::first(tt)))
})


test_that("tsdiff returns FALSE if two timeseries are equal",  {
  a <- b <- ts(runif(10), start = c(1990, 1), frequency = 1)
  expect_true(!tsdiff(a, b))
  a <- b <- ts(runif(10), start = c(1990, 1), frequency = 4)
  expect_true(!tsdiff(a, b))
  a <- b <- ts(runif(10), start = c(1990, 1), frequency = 12)
  expect_true(!tsdiff(a, b))
})

test_that("tsdiff returns TRUE if two timeseries differ",  {
  a <- ts(runif(10), start = c(1990, 1), frequency = 1)
  b <- ts(runif(10), start = c(1990, 1), frequency = 1)
  expect_true(tsdiff(a, b))
  a <- ts(runif(10), start = c(1990, 1), frequency = 4)
  b <- ts(runif(10), start = c(1990, 1), frequency = 4)
  expect_true(tsdiff(a, b))
  a <- ts(runif(10), start = c(1990, 1), frequency = 12)
  b <- ts(runif(10), start = c(1990, 1), frequency = 12)
  expect_true(tsdiff(a, b))
})

test_that("tsdiff returns TRUE if two timeseries have different index",  {
  a <- ts(runif(10), start = c(1990, 1), frequency = 1)
  b <- ts(runif(10), start = c(1990, 1), frequency = 12)
  expect_true(tsdiff(a, b))
  a <- ts(runif(10), start = c(1990, 1), frequency = 4)
  b <- ts(runif(10), start = c(1990, 1), frequency = 12)
  expect_true(tsdiff(a, b))
  a <- ts(runif(10), start = c(1990, 1), frequency = 1)
  b <- ts(runif(10), start = c(1990, 1), frequency = 4)
  expect_true(tsdiff(a, b))
})

test_that("tsdiff returns TRUE if two timeseries have different dimensions",  {
  v1 <- c(1, 2, 3, 4, 5)
  v2 <- c(v1, 6)
  a <- ts(v1, start = c(1990, 1), frequency = 1)
  b <- ts(v2, start = c(1990, 1), frequency = 1)
  expect_true(tsdiff(a, b))
  a <- ts(v1, start = c(1990, 1), frequency = 4)
  b <- ts(v2, start = c(1990, 1), frequency = 4)
  expect_true(tsdiff(a, b))
  a <- ts(v1, start = c(1990, 1), frequency = 12)
  b <- ts(v2, start = c(1990, 1), frequency = 12)
  expect_true(tsdiff(a, b))
})


test_that(" to_data_frame correctly converts a single obs timeseries", {
  anno <- 1990
  period <- 1
  freq <- 4
  dati <- 1

  tt <- ts(dati, start = c(anno, period), frequency = freq)
  x <-  to_data_frame(tt)

  expect_equal(x[, "anno"], anno)
  expect_equal(x[, "freq"], freq)
  expect_equal(x[, "periodo"], period)
  expect_equal(x[, "dati"], "[1]")
})

test_that("to_data_frame converts a single obs ts produced by formula", {
  anno <- 1990
  period <- 1
  freq <- 4
  dati <- 1

  g <- GrafoDB("test")
  on.exit(delete_graph("test"))

  g["A"] <- g["B"] <- ts(c(1, 2, 3), start = c(anno, period), frequency = freq)
  g["C"] <- function(A, B) { # nolint voglio solo l'ultimo periodo
    C <- window(A + B, start = c(1990, 1 + 2)) # nolint
  }

  x <-  to_data_frame(g$C)
  expect_equal(x[, "anno"], anno)
  expect_equal(x[, "freq"], freq)
  expect_equal(x[, "periodo"], period + 2)
  expect_equal(x[, "dati"], "[6]")

  expect_equal(length(g$C), 1)
})
