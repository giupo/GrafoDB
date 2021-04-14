context("Persistence functions")

setup <- function(name) {
  g <- GrafoDB(name)
  g["A"] <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  g["B"] <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C = A + B # nolint
  }

  g["D"] <- stats::ts(c(NA, 1, NA), start = c(1990, 1), frequency = 4)

  g <- saveGraph(g)
  g
}

g <- setup("test")

test_that("Posso salvare e ricaricare da un file", {
  path <- tempfile()
  saveBinary(g, path)
  x <- readBinary(path)
  expect_true(is.grafodb(x))
  unlink(path)
})

test_that("I can handle NaN despite JsonCpp, jsonlite, IEEE754", {
  ## il problema qui e' che quando serializzo python giustamente
  ## usa 'NaN' per i missing. mentre C++/R preferiscono 'null'
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  con <- build_connection()
  on.exit(disconnect(con))
  DBI::dbExecute(
    con,
    "update dati set dati = replace(dati, 'null', '\"NaN\"') where tag='test'")

  g <- saveGraph(g)
  g1 <- GrafoDB("test")
  expect_true(any(is.na(g[["D"]])))
  expect_true(length(g[["D"]]) > 0)
})

setup <- function(tag) {
  g <- GrafoDB(tag)
  g["A"] <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  g["B"] <- stats::ts(1:10, start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C = A + B # nolint
  }

  g["D"] <- function(B) { # nolint
    D = B * 2 # nolint
  }

  g
}

test_that("I can save a graph over another existing graph", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  tag_a <- "test2"
  tag_b <- "test3"
  ga <- setup(tag_a)
  gb <- setup(tag_b)

  ga <- saveGraph(ga)
  gb <- saveGraph(gb)

  ga <- saveGraph(ga, tag_b)

  g <- GrafoDB(tag_b)
  expect_true(all(g$B == gb$B))
})

delete_graph("test")

context("Persistence functions [internal functions]")

test_that("load_table stops if table_name doesn't exists", {
  expect_error(load_table("nonesisto", "nonesisto"))
})

test_that("Load* yields an empy dataframe in case of error", {
  ## I don't like this at all.

  with_mock(
    "GrafoDB:::load_table" = function(...) stop("my error"), {
      x <- load_data("nonsense")
      expect_is(x, "data.frame")
      expect_equal(nrow(x), 0)

      x <- load_edges("nonesisto")
      expect_is(x, "data.frame")
      expect_equal(nrow(x), 0)

      x <- load_formulas("nonesisto")
      expect_is(x, "data.frame")
      expect_equal(nrow(x), 0)
    })
})

test_that("need_resync returns true if GrafoNeeds a resync", {
  g <- setup("test")
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })

  g <- saveGraph(g)

  g1 <- GrafoDB("test")
  g2 <- GrafoDB("test")
  g2["A"] <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  g2 <- saveGraph(g2)
  expect_true(need_resync(g1))
})

test_that("do_history preserves last_updated", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g["A"] <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  g1 <- saveGraph(g)

  g1["B"] <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  g1 <- saveGraph(g1)
  p <- GrafoDB("testp1")
  expect_true("A" %in% names(p))
  expect_false(g1@timestamp == g@timestamp)
  expect_equal(g@timestamp, p@timestamp)
})


test_that("load_metadati calls load_table", {
  skip_if_not_installed("mockery")
  mock_load_table <- mockery::mock(TRUE)
  mockery::stub(load_metadata, "load_table", mock_load_table)
  expect_error(load_metadata("x"), NA)
  expect_called(mock_load_table, 1)
})

test_that("load_grafi handles when con is NULL", {
  skip_if_not_installed("mockery")
  build_connection_mock <- mock(TRUE)
  disconnect_mock <- mock(TRUE)
  db_read_table_mock <- mock(TRUE)

  mockery::stub(load_grafi, "build_connection", build_connection_mock)
  mockery::stub(load_grafi, "disconnect", disconnect_mock)
  mockery::stub(load_grafi, "DBI::dbReadTable", db_read_table_mock)

  expect_error(load_grafi(), NA)
  expect_called(build_connection_mock, 1)
  expect_called(disconnect_mock, 1)
  expect_called(db_read_table_mock, 1)
})

test_that("load_grafi handles when con is not NULL", {
  skip_if_not_installed("mockery")
  build_connection_mock <- mock(TRUE)
  disconnect_mock <- mock(TRUE)
  db_read_table_mock <- mock(TRUE)

  mockery::stub(load_grafi, "build_connection", build_connection_mock)
  mockery::stub(load_grafi, "disconnect", disconnect_mock)
  mockery::stub(load_grafi, "DBI::dbReadTable", db_read_table_mock)

  expect_error(load_grafi(con = 1), NA)
  expect_called(build_connection_mock, 0)
  expect_called(disconnect_mock, 0)
  expect_called(db_read_table_mock, 1)
})