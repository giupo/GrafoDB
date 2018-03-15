context("Metadati")

setup <- function(tag) {
  dbSettings(TRUE)
  flog.debug("Nome dell'env dall'env: %s", Sys.getenv("GRAFODB_ENV"))
  g <- GrafoDB(tag)

  g["A"] <- g["B"] <- ts(c(0,0,0), start=c(1990,1), freq=1)
  g["C"] <- function(A, B) {
    C = (A + 1) * (B + 2)
  }
  saveGraph(g)
  
  setMeta(g, "A", "KEY", "VALUE1")
  setMeta(g, "A", "KEY", "VALUE2")
  setMeta(g, "B", "KEY", "VALUE1")
  g
}


test_that("posso caricare tutti i metadati del grafo", {
  on.exit({
    elimina("test")
  })
  g <- setup("test")
  with_mock(
    'RCurl::getURL' = function(...) "{}", {
      meta <- getMeta(g)
      expect_true(is.data.frame(meta))
      expect_equal(nrow(meta), 3)
    })
})

test_that("posso ottenere i metadati per una serie", {
  on.exit({
    elimina("test") 
  })
  g <- setup("test")
  with_mock(
    'RCurl::getURL' = function(...) "{}", {
      m <- getMeta(g, "A")
      expect_true(is.data.frame(m))
      expect_equal(nrow(m), 2)
    })
})

test_that("posso ottenere i valori di un metadato per singola serie", {
   on.exit({
    elimina("test")
  })
  g <- setup("test")
  with_mock(
    'RCurl::getURL' = function(...) "{}", {
      v <- getMeta(g, "A", "KEY")
      expect_true(is.character(v))
      expect_true(all(c("VALUE1", "VALUE2") %in% v ))
    })
})

test_that("Posso cercare i numeri direttamente nel grafo", {
  on.exit({
    elimina("test")
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
    elimina("test")
  })
  g <- setup("test")
  deleteMeta(g, "A", "KEY", "VALUE1")
  with_mock(
    'RCurl::getURL' = function(...) "{}", {
      v <- getMeta(g, "A", "KEY")
      expect_true(is.character(v))
      expect_true(all(c("VALUE2") %in% v ))
    })
})

test_that("If I get an erro with DB, deleteMeta fails", {
  on.exit({
    for(tag in rilasci("test")$tag) elimina(tag)
  })

  g <- setup("test")
  with_mock(
    'DBI::dbCommit' = function(...) stop("error test"), {
      expect_error(deleteMeta(g, "A", "KEY", "VALUE1"), "error test")
    })
})

test_that("Ottengo un errore se accade un errore sul DB nella lettura di Metadati", {
  skip_if_not(require(mockery), "mockery required")
  on.exit({
    elimina("test")
  })
  g <- setup("test")
  stub(.getMeta, 'getSQLbyKey', function(...) stop("error"))
  expect_error(.getMeta(g, "A", "KEY"), "error")
})

test_that("I get nothing if there are no metadata values for a key", {
  on.exit(elimina("test"))
  g <- setup("test")
  expect_equal(length(getMeta(g, "A", "NONESISTO")), 0)
})

test_that("setMeta has a warning each time you set an already existing meta", {
  skip_if_not(require(mockery), "mockery required")
  on.exit({
    elimina("test")
  })
  g <- setup("test")
  stub(.setMeta, 'warning', function(...) stop("dc"))

  expect_error(.setMeta(g, "A", "KEY", "VALUE1"))
})

test_that("setMeta su una serie inesistente produce un errore", {
  on.exit({
    elimina("test")
  })
  g <- setup("test")
  expect_error(setMeta(g, "NONESISTO", "KEY", "VALUE1"))
})

context("Metadati [internal functions]")

test_that(".lookupFormula works as expected", {
  g <- setup("test")
  on.exit(elimina("test"))
  expect_equal(length(.lookup_formula(g, "*")), 1)
  expect_equal(.lookup_formula(g, "*"), "C")
})

test_that(".keys returns keys of metadata", {
  g <- setup("test")
  on.exit(elimina("test"))
  x <- GrafoDB:::.keys(g)
  expect_is(x, "data.frame")
  expect_equal(x$key, "KEY")
})

test_that(".values returns values of all metadata, or per key basis", {
  g <- setup("test")
  on.exit(elimina("test"))

  v <- GrafoDB:::.values(g)
  expect_is(v, "character")
  expect_equal(v, c("VALUE1", "VALUE2"))

  v <- GrafoDB:::.values(g, key="KEY")
  expect_is(v, "character")
  expect_equal(v, c("VALUE1", "VALUE2"))
})

test_that("I get additional TICKET metadata from issue tracker", {
  g <- setup("test")
  on.exit({
    for(tag in rilasci("test")$tag) elimina(tag)
  })

  require(mockery)
  stub(.getMetadata, 'get_tickets_urls_for_name', function(x, name) {
    expect_equal(name, "A")
    "http://unusedhost/ticket/1"
  })
  stub(.getMetadata, 'ticket', function(id) {
    expect_equal(id, 2)
    "[1,2,3,{'status':'open'}]"
    df <- .getMetadata(g, "A") # qui si solleva un warning
    expect_true("TICKET" %in% df$key)
  })
})

test_that("I can remove all metadata with a single key entry", {
    g <- setup("test")
    on.exit({
        for(tag in rilasci("test")$tag) elimina(tag)
    })

    
    deleteMeta(g, "A", "KEY")
    with_mock(
      'RCurl::getURL' = function(...) "{}", {
        v <- getMeta(g, "A")
        expect_equal(nrow(v), 0)
    })    
})
