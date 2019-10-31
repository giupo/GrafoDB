context("Util functions")

test_that("to.data.frame converte correttamente una serie", {  
  tt <- ts(runif(10), start=c(1990,1), frequency=4)
  df <- to.data.frame(tt, "TEST")
  expect_true(identical(tt, from.data.frame(df)[[df$name]]))
})

test_that("to.data.frame converte correttamente vettori", {  
  tt <- runif(10)
  df <- to.data.frame(tt, "TEST")
  expect_true(identical(tt, from.data.frame(df)$TEST))
})

test_that("to.data.frame converte correttamente scalari", {  
  tt <- 1
  df <- to.data.frame(tt, "TEST")
  expect_true(identical(tt, from.data.frame(df)$TEST))
})


test_that("to.data.frame converte correttamente missing", {  
  tt <- NA
  df <- to.data.frame(tt, "TEST")
  expect_true(identical(tt, from.data.frame(df)$TEST))
})

test_that("to.data.frame converte correttamente vettori di missing", {  
  tt <- c(NA, NA)
  df <- to.data.frame(tt, "TEST")
  expect_true(identical(tt, from.data.frame(df)$TEST))
})

test_that(".declutter_functions removes correctly functions from it's definitions", {
  f <- "function(A,B,C) { A = 1 }"
  f <- .declutter_function(f)
  expect_equal(f, "A = 1")

  f <- function(A,B,C) {
    A = A+B+C
  }

  f <- .declutter_function(f)
  expect_equal(f, "A = A+B+C")
})

test_that(".decluter_functions preserves commnets", {
  f <- function(A) {
    # comment here
    A
  }

  f <- .declutter_function(f)
  expect_equal(f, "# comment here\n    A")

  f <- "function(A) { # comment here\nA}"
  f <- .declutter_function(f)
  expect_equal(f, "# comment here\nA")
})

test_that("checkDAG raises an exception with a cycle in network", {
  g <- graph.empty(directed=TRUE)
  g <- g + vertex("A")
  g <- g + vertex("B")
  g <- g + edge("A", "B")
  expect_error(checkDAG(g), NA)
  g <- g + edge("B", "A")
  expect_error(checkDAG(g), "Cycles found")
})

test_that("elimina deletes a GrafoDB", {
  for(tag in rilasci("test")$tag) elimina(tag)
  on.exit({
    for(tag in rilasci("test")$tag) elimina(tag)
  })
  g <- GrafoDB("test")
  df <- rilasci("test")
  expect_equal(nrow(df), 1)
})

test_that("elimina handles exceptions", {
  on.exit({
    for(tag in rilasci("test")$tag) elimina(tag)
  })

  g <- GrafoDB("test")
  with_mock(
    'DBI::dbCommit'= function(...) stop("error test"), {
      expect_error(elimina(g), "error test")
      expect_error(elimina("test"), "error test")
    })
})

test_that("schemaFileFromEnv returns a consistent file", {
  expect_true(is.list(dbSettings(TRUE)))
  test_file <- schemaFileFromEnv("test")
  prod_file <- schemaFileFromEnv("prod")
  collaudo_file <- schemaFileFromEnv("collaudo")

  expect_equal(basename(test_file), "schema-SQLite.sql")
  expect_equal(basename(prod_file), "schema-PostgreSQL.sql")
  expect_equal(basename(collaudo_file), "schema-PostgreSQL.sql")
})
