context("Metadata")

setup <- function(tag) {
  db_settings(TRUE)
  debug("Nome dell'env: %s", Sys.getenv("GRAFODB_ENV"))
  g <- GrafoDB(tag)

  g["A"] <- g["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 1)
  g["C"] <- function(A, B) { # nolint
    C = (A + 1) * (B + 2) # nolint
  }
  g <- saveGraph(g)

  setMeta(g, "A", "KEY", "VALUE1")
  setMeta(g, "A", "KEY", "VALUE2")
  setMeta(g, "B", "KEY", "VALUE1")
  g
}


test_that("posso caricare tutti i metadati del grafo", {
  on.exit({
    delete_graph("test")
  })
  g <- setup("test")
  meta <- getMeta(g)
  expect_true(is.data.frame(meta))
  expect_equal(nrow(meta), 3)

})

test_that("posso ottenere i metadati per una serie", {
  on.exit({
    delete_graph("test")
  })
  g <- setup("test")
  m <- getMeta(g, "A")
  expect_true(is.data.frame(m))
  expect_equal(nrow(m), 2)
})

test_that("posso ottenere i valori di un metadato per singola serie", {
  on.exit({
    delete_graph("test")
  })
  g <- setup("test")
  v <- getMeta(g, "A", "KEY")
  expect_true(is.character(v))
  expect_true(all(c("VALUE1", "VALUE2") %in% v))
})

test_that("Posso cercare i numeri direttamente nel grafo", {
  on.exit({
    delete_graph("test")
  })
  g <- setup("test")

  ret <- lookup(g, 2)
  expect_true("C" %in% ret)
  expect_true(! "A" %in% ret)
  ret <- lookup(g, 0)
  expect_true("A" %in% ret)
  expect_true("B" %in% ret)
  expect_true(! "C" %in% ret)
})

test_that("Posso cancellare Metadati", {
  on.exit({
    delete_graph("test")
  })
  g <- setup("test")
  deleteMeta(g, "A", "KEY", "VALUE1")
  v <- getMeta(g, "A", "KEY")
  expect_true(is.character(v))
  expect_true(all(c("VALUE2") %in% v))
})

test_that("If I get an erro with DB, deleteMeta fails", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })

  g <- setup("test")
  with_mock(
    "DBI::dbCommit" = function(...) stop("error test"), {
      expect_error(deleteMeta(g, "A", "KEY", "VALUE1"), "error test")
    })
})

test_that("got error if there's an error reading from DB", {
  skip_if_not(require(mockery), "mockery required")
  on.exit({
    delete_graph("test")
  })
  g <- setup("test")
  stub(get_meta_impl, "sql_by_key", function(...) stop("error"))
  expect_error(get_meta_impl(g, "A", "KEY"), "error")
})

test_that("I get nothing if there are no metadata values for a key", {
  on.exit(delete_graph("test"))
  g <- setup("test")
  expect_equal(length(getMeta(g, "A", "NONESISTO")), 0)
})

test_that("setMeta has a warning each time you set an already existing meta", {
  skip_if_not(require(mockery), "mockery required")
  on.exit({
    delete_graph("test")
  })
  g <- setup("test")
  stub(set_meta_impl, "warning", function(...) stop("dc"))

  expect_error(set_meta_impl(g, "A", "KEY", "VALUE1"))
})

test_that("setMeta su una serie inesistente produce un errore", {
  on.exit({
    delete_graph("test")
  })
  g <- setup("test")
  expect_error(setMeta(g, "NONESISTO", "KEY", "VALUE1"))
})

context("Metadata [internal functions]")

test_that(".lookupFormula works as expected", {
  g <- setup("test")
  on.exit(delete_graph("test"))
  expect_equal(length(lookup_formula_impl(g, "*")), 1)
  expect_equal(lookup_formula_impl(g, "*"), "C")
})

test_that("keys_impl returns keys of metadata", {
  g <- setup("test")
  on.exit(delete_graph("test"))
  x <- GrafoDB:::keys_impl(g)
  expect_is(x, "data.frame")
  expect_equal(x$key, "KEY")
})

test_that(".values returns values of all metadata, or per key basis", {
  g <- setup("test")
  on.exit(delete_graph("test"))

  v <- GrafoDB:::values_by_key_impl(g)
  expect_is(v, "character")
  expect_equal(v, c("VALUE1", "VALUE2"))

  v <- GrafoDB:::values_by_key_impl(g, key = "KEY")
  expect_is(v, "character")
  expect_equal(v, c("VALUE1", "VALUE2"))
})

test_that("I can remove all metadata with a single key entry", {
  g <- setup("test")
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })

  deleteMeta(g, "A", "KEY")
  v <- getMeta(g, "A")
  expect_equal(nrow(v), 0)
})


test_that("I can search for metadata values from names and keys", {
  g <- setup("test")
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })


  x <- values_for(g, c("A", "B"), "KEY")
  expect_equal(nrow(x), 3)
})


test_that("values_for returns all metadata without params", {
  g <- setup("test")
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  x <- values_for(g)
  expect_equal(nrow(x), 3)
})


test_that("values_for raise an error if any of the params are NULL", {
  g <- setup("test")
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  expect_error(values_for(g, name = NULL), "name cannot be null")
  expect_error(values_for(g, key = NULL), "key cannot be null")
})
