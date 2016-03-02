context("Basic Operations")

test_that("Posso istanziare un GrafoDB", {
  g <- GrafoDB("test")
  expect_true(is.grafodb(g))
})

elimina("test")

test_that("Posso istanziare un GrafoDB con un tag", {
  g <- GrafoDB("test")
  expect_true(is.grafodb(g))
})

elimina("test")

test_that("Posso impostare una timeseries con nome nel GrafoDB", {
  g <- GrafoDB("test")
  g["A"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  expect_true("A" %in% names(g))
  expect_true(is.bimets(g[["A"]]))
})

elimina("test")

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

elimina("test")

test_that("Posso usare delle formule per definire le serie", {
  g <- GrafoDB("test")
  g["A"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["B"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B    
  }
})

elimina("test")

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

elimina("test")

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
})


elimina("test1")
elimina("test")

test_that("names su un grafo vuoto torna un array vuoto", {
  g <- GrafoDB("test")
  expect_equal(length(names(g)), 0)
  elimina("test")
})

elimina("test")

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

elimina("test")

test_that("a tag with 'p' returns the tag with the ordinal", {
  g <- GrafoDB("test")
  expect_equal(g@ordinal, 0)
  expect_equal(g@tag, "test")

  g <- GrafoDB("testp1")
  expect_equal(g@ordinal, 1)
  expect_equal(g@tag, "test")
})

elimina("test")

test_that("Cascade subsetting works", {
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- TSERIES(c(0,0,0), START=c(1990,1), FREQ=4)
  expect_true("A" %in% names(g))
  expect_true("B" %in% names(g))
})

test_that("Saving preserves network and nodes", {
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- TSERIES(c(0,0,0), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B
  }
  ## ora salvo, e vediamo cosa succede
  g <- saveGraph(g)
  
  expect_true("A" %in% names(g))
  expect_true("B" %in% names(g))
  expect_true("A" %in% upgrf(g, "C", livello=1))
  expect_true("B" %in% upgrf(g, "C", livello=1))
})

elimina("test")

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

  expect_true("B" %in% names(g))
  expect_true("C" %in% names(g))
  expect_true("A" %in% names(g))
  
  g["C"] <- function(A) {
    C=A
  }
  
  expect_true("A" %in% upgrf(g, "C"))
  expect_true(!"B" %in% upgrf(g, "C"))
  expect_equal(length(upgrf(g, "C")), 1)
})

elimina("test")

test_that("I can cast a empty GrafoDB to a Dataset", {
  g <- GrafoDB("test")
  d <- as.dataset(g)
  expect_true(is.dataset(d))
  expect_true(all(names(d) %in% names(g)))
  for(name in names(d)) {
    d1 <- d[[name]]
    g1 <- g[[name]]
    expect_equal(d1, g1)
  }
})

elimina("test")

test_that("I can cast a GrafoDB to a Dataset", {
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- TSERIES(c(0,0,0), START=c(1990,1), FREQ=4)
  g["C"] <- function(A) {
    C=A
  }
  d <- as.dataset(g)
  expect_true(is.dataset(d))
  expect_true(all(c("A", "B", "C") %in% names(d)))
}) 

elimina("test")

test_that("Posso passare non timeseries", {
  g <- GrafoDB("test")
  g["A"] <- TSERIES(c(0,0,0), START=c(1990,1), FREQ=4)
  g["B"] <- TSERIES(c(1,1,1), START=c(1990,1), FREQ=4)
  g["periodo"] <- c(1990,2)
  g["C"] <- function(A,B, periodo) {
    C=TSJOIN(A,B, JPRD=periodo)
  }
  expect_equal(g[["periodo"]], c(1990,2))
})

elimina("test")

test_that("posso salvare non timeseries", {
  g <- GrafoDB("test")
  g["A"] <- TSERIES(c(0,0,0), START=c(1990,1), FREQ=4)
  g["B"] <- TSERIES(c(1,1,1), START=c(1990,1), FREQ=4)
  g["periodo"] <- c(1990,2)
  g["C"] <- function(A,B, periodo) {
    C=TSJOIN(A,B, JPRD=periodo)
  }
  g <- saveGraph(g)
  expect_equal(g[["periodo"]], c(1990,2))
})

elimina("test")

test_that("posso copiare un grafo da tag a tag", {
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- TSERIES(c(0,0,0), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B
  }
  setMeta(g, "A", "KEY", "VALUE")
  g = saveGraph(g)
  saveGraph(g, "test1")
  g1 = GrafoDB("test1")
  expect_true(all(names(g) %in% names(g1)))
  expect_equal(searchNode(g, "KEY", "VALUE"), "A")
  expect_equal(searchNode(g1, "KEY", "VALUE"), "A")
})

elimina("test")
elimina("test1")

test_that("posso memorizzare stringhe", {
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- TSERIES(c(0,0,0), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B
  }
  g["archivio"] = "cippalippa"
  saveGraph(g)

  g1 <- GrafoDB("test")
  expect_equal(g1[["archivio"]], "cippalippa")
})

elimina("test")

test_that("isLeaf torna true per serie foglia", {
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- TSERIES(c(0,0,0), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B
  }
  expect_true(isLeaf(g, "C"))
})

elimina("test")

test_that("isLeaf torna false per una serie non foglia", {
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- TSERIES(c(0,0,0), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B
  }
  expect_true(!isLeaf(g, "A"))
  expect_true(!isLeaf(g, "B"))
})

elimina("test")

test_that("isRoot torna false per serie foglia", {
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- TSERIES(c(0,0,0), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B
  }
  expect_true(!isRoot(g, "C"))
})

elimina("test")

test_that("isRoot torna true per una serie non foglia", {
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- TSERIES(c(0,0,0), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B
  }
  expect_true(isRoot(g, "A"))
  expect_true(isRoot(g, "B"))
})

elimina("test")

test_that("Posso editare una serie esistente aggiungendo una dipendenza esistente", {
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- TSERIES(c(0,0,0), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B
  }
  g["D"] <- function(A, C) {
    D = A + C
  }
  g <- saveGraph(g)
  
  g["D"] <- function(A, B, C) {
    D = A + B + C
  }
})

elimina("test")

test_that("Posso subsettare con il $ (dollaro)", {
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- TSERIES(c(0,0,0), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B
  }
  g["D"] <- function(A, C) {
    D = A + C
  }
  d <- g$D
  expect_equal(g$D, g[["D"]])
  expect_equal(g$A, g[["A"]])
  expect_equal(g$C, g[["C"]])
  
})

elimina("test")
