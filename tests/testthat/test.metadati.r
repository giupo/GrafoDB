context("Metadati")

dbSettings(TRUE)
elimina("test")

setup <- function(tag) {
  g <- GrafoDB(tag)
  g["A"] <- g["B"] <- ts(c(0,0,0), start=c(1990,1), freq=1)
  g["C"] <- function(A, B) {
    C = (A + 1) * (B + 2)
  }
  saveGraph(g)
  
  setMeta(g, "A", "KEY", "VALUE1")
  setMeta(g, "A", "KEY", "VALUE2")
  setMeta(g, "B", "KEY", "VALUE1")
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

test_that("Ottengo un errore se accade un errore sul DB nella cancellazione di  Metadati", {
  on.exit({
    elimina("test")
  })
  g <- setup("test")
  with_mock(
    getSQLbyKey = function(...) stop("error"), {
      expect_error(getMeta(g, "A", "KEY"), "error")
    })
})

test_that("setMeta has a warning each time you set an already existing meta", {
  on.exit({
    elimina("test")
  })
  g <- setup("test")
  with_mock(
    # hack to regenerate the warning
    'base::warning' = function(...) stop(...), {
      expect_error(setMeta(g, "A", "KEY", "VALUE1"))
    })
})

test_that("setMeta su una serie inesistente produce un errore", {
  on.exit({
    elimina("test")
  })
  g <- setup("test")
  expect_error(setMeta(g, "NONESISTO", "KEY", "VALUE1"))
})
