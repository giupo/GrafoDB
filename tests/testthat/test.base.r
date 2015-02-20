context("Basic Operations")

test_that("Posso istanziare un GrafoDB", {
  g <- GrafoDB("test")
  expect_true(is.grafodb(g))
})

test_that("Posso istanziare un GrafoDB con un tag", {
  g <- GrafoDB("test")
  expect_true(is.grafodb(g))
})

test_that("Posso impostare una timeseries con nome nel GrafoDB", {
  g <- GrafoDB("test")
  g["A"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  expect_true("A" %in% names(g))
  expect_true(is.bimets(g[["A"]]))
})

test_that("Posso impostare piu' timeseries con nome nel GrafoDB", {
  g <- GrafoDB("test")
  g["A"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["B"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["C"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  expect_true("A" %in% names(g))
  expect_true(is.bimets(g[["A"]]))
  expect_true("B" %in% names(g))
  expect_true(is.bimets(g[["B"]]))
  expect_true("C" %in% names(g))
  expect_true(is.bimets(g[["C"]]))

  expect_true(all(g[["A"]] != g[["B"]]))
  expect_true(all(g[["A"]] != g[["C"]]))
  expect_true(all(g[["B"]] != g[["C"]]))
})

test_that("Posso usare delle formule per definire le serie", {
  g <- GrafoDB("test")
  g["A"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["B"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B    
  }  
})

test_that("Posso cercare le serie con metadati", {
  g <- GrafoDB("test")
  g["A"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["B"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B    
  }

  g <- setMeta(g, "A", "key", "value")
  g <- setMeta(g, "B", "key", "value")
  g <- setMeta(g, "C", "key", "value1")
  
  res <- lookup(g, "key", "value")
  expect_equal(length(res), 2)
  expect_true(all(c("A", "B") %in% res))  
})

test_that("Posso salvare il grafo sul database", {
  g <- GrafoDB("test")
  g["A"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["B"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B    
  }

  g <- setMeta(g, "A", "key", "value")
  g <- setMeta(g, "B", "key", "value")
  g <- setMeta(g, "C", "key", "value1")

  g1 <- saveGraph(g, "test1")

  expect_true(all(c("A", "B", "C") %in% names(g1)))  
  elimina("test1")
})

test_that("names su un grafo vuoto torna un array vuoto", {
  elimina("test")
  g <- GrafoDB("test")
  expect_equal(length(names(g)), 0)  
})

test_that("subset with datasets", {
  g <- GrafoDB("test")
  g["A"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["B"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B    
  }
  
  g <- setMeta(g, "A", "key", "value")
  g <- setMeta(g, "B", "key", "value")
  g <- setMeta(g, "C", "key", "value1")
   
  expect_true(all(c("A", "B", "C") %in% names(g)))  
  
  ds <- Dataset()
  ds["A"] <- TSERIES(c(-1,-2,-3), START=c(1990,1), FREQ=4)
  ds["B"] <- TSERIES(c(0,0,0), START=c(1990,1), FREQ=4)

  g[names(ds)] <- ds

  expect_equal(g[["A"]], ds[["A"]])
  expect_equal(g[["B"]], ds[["B"]])
  elimina("test")
})

test_that("a tag with 'p' returns the tag with the ordinal", {
  g <- GrafoDB("test")
  expect_equal(g@ordinal, 0)
  expect_equal(g@tag, "test")

  g <- GrafoDB("testp1")
  expect_equal(g@ordinal, 1)
  expect_equal(g@tag, "test")
  elimina("test")
})

test_that("posso rimuovere archi", {
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- TSERIES(c(0,0,0), START=c(1990,1), FREQ=4)
  g["C"] <- function(A) {
    C=A
  }
  expect_true("A" %in% upgrf(g, "C"))
  expect_true(!"B" %in% upgrf(g, "C"))
  expect_equal(length(upgrf(g, "C")), 1)

  g["C"] <- function(B) {
    C=B
  }
  
  expect_true(!"A" %in% upgrf(g, "C")) 
  expect_true("B" %in% upgrf(g, "C"))
  expect_equal(length(upgrf(g, "C")), 1)

  ## ora salvo, e vediamo cosa succede
  g <- saveGraph(g)

  g["C"] <- function(A) {
    C=A
  }
  expect_true("A" %in% upgrf(g, "C"))
  expect_true(!"B" %in% upgrf(g, "C"))
  expect_equal(length(upgrf(g, "C")), 1)
  
  elimina("test")
})

test_that("I can cast a empty GrafoDB to a Dataset", {
  g <- GrafoDB("test")
  d <- as.dataset(g)
  expect_true(is.dataset(d))
  elimina("test")
})

test_that("I can cast a GrafoDB to a Dataset", {
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- TSERIES(c(0,0,0), START=c(1990,1), FREQ=4)
  g["C"] <- function(A) {
    C=A
  }
  d <- as.dataset(g)
  expect_true(is.dataset(d))
  expect_true(all(c("A", "B", "C") %in% names(d)))
  elimina("test")
}) 
