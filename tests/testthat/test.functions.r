context("Utils functions")

test_that(" to_data_frame converte correttamente una serie", {
  tt <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  df <-  to_data_frame(tt, "TEST")
  expect_true(identicalts(tt, from_data_frame(df)[[df$name]]))
})

test_that(" to_data_frame converte correttamente vettori", {
  tt <- runif(10)
  df <-  to_data_frame(tt, "TEST")
  expect_true(identicalts(tt, from_data_frame(df)$TEST))
})

test_that(" to_data_frame converte correttamente scalari", {
  tt <- 1
  df <-  to_data_frame(tt, "TEST")
  expect_true(identicalts(tt, from_data_frame(df)$TEST))
})


test_that(" to_data_frame converte correttamente missing", {
  tt <- NA
  df <-  to_data_frame(tt, "TEST")
  expect_true(identical(tt, from_data_frame(df)$TEST))
})

test_that(" to_data_frame converte correttamente vettori di missing", {
  tt <- c(NA, NA)
  df <-  to_data_frame(tt, "TEST")
  expect_true(identical(tt, from_data_frame(df)$TEST))
})

test_that("declutter_functions removes correctly functions from it's def", {
  f <- "function(A, B,C) { A = 1 }"
  f <- declutter_function(f)
  expect_equal(f, "A = 1")

  f <- function(A, B, C) {
    A = A + B + C
  }

  f <- declutter_function(f)
  expect_equal(f, "A = A + B + C")
})

test_that(".decluter_functions preserves comments", {
  f <- function(A) {
    # comment here
    A
  }

  f <- declutter_function(f)
  expect_equal(f, "# comment here\n    A")

  f <- "function(A) { # comment here\nA}"
  f <- declutter_function(f)
  expect_equal(f, "# comment here\nA")
})

test_that("assert_dag raises an exception with a cycle in network", {
  g <- igraph::graph.empty(directed = TRUE)
  g <- g + igraph::vertex("A")
  g <- g + igraph::vertex("B")
  g <- g + igraph::edge("A", "B")
  expect_error(assert_dag(g), NA)
  g <- g + igraph::edge("B", "A")
  expect_warning(expect_error(assert_dag(g), "Cycles found"),
    "partial result is returned")
})

test_that("delete deletes a GrafoDB", {
  for (tag in rilasci("test")$tag) delete_graph(tag)
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  df <- rilasci("test")
  expect_equal(nrow(df), 1)
})

test_that("delete handles exceptions", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })

  g <- GrafoDB("test")
  with_mock(
    "DBI::dbCommit" = function(...) stop("error test"), {
      expect_error(delete_graph(g), "error test")
      expect_error(delete_graph("test"), "error test")
    })
})

test_that("schema_from_env returns a consistent file", {
  expect_true(is.list(db_settings(TRUE)))
  test_file <- schema_from_env("test")
  prod_file <- schema_from_env("prod")
  collaudo_file <- schema_from_env("collaudo")

  expect_equal(basename(test_file), "schema-SQLite.sql")
  expect_equal(basename(prod_file), "schema-PostgreSQL.sql")
  expect_equal(basename(collaudo_file), "schema-PostgreSQL.sql")
})
