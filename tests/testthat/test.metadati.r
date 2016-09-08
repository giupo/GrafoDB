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
  g <- setup("test")
  meta <- getMeta(g)
  expect_true(is.data.frame(meta))
  expect_equal(nrow(meta), 3)  
})

elimina("test")

test_that("posso ottenere i metadati per una serie", {
  g <- setup("test")
  m <- getMeta(g, "A")
  expect_true(is.data.frame(m))
  expect_equal(nrow(m), 2)
})

elimina("test")

test_that("posso ottenere i valori di un metadato per singola serie", {
  g <- setup("test")
  v <- getMeta(g, "A", "KEY")
  expect_true(is.character(v))
  expect_true(all(c("VALUE1", "VALUE2") %in% v ))
})

elimina("test")

test_that("Posso cercare i numeri direttamente nel grafo", {
  g <- setup("test")
  ret <- lookup(g, 2)
  expect_true("C" %in% ret)
  expect_true(! "A" %in% ret)
  ret <- lookup(g, 0)
  expect_true("A" %in% ret)
  expect_true("B" %in% ret)
  expect_true(! "C" %in% ret)
})

for(tag in rilasci("test")$tag) elimina(tag)
