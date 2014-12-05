context("Basic Operations")

test_that("Posso istanziare un GrafoDB", {
  g <- GrafoDB()
  expect_true(is.grafodb(g))
})

test_that("Posso istanziare un GrafoDB con un tag", {
  g <- GrafoDB("test")
  expect_true(is.grafodb(g))
})

test_that("Posso impostare una timeseries con nome nel GrafoDB", {
  g <- GrafoDB()
  g["A"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  expect_true("A" %in% names(g))
  expect_true(is.bimets(g[["A"]]))
})

test_that("Posso impostare piu' timeseries con nome nel GrafoDB", {
  g <- GrafoDB()
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
  g <- GrafoDB()
  g["A"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["B"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B    
  }  
})

test_that("Posso cercare le serie con metadati", {
  g <- GrafoDB()
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

  saveGraph(g, "test1")

  g1 <- GrafoDB("test1")

  expect_true(all(c("A", "B", "C") %in% names(g1)))  
  elimina("test1")
})

test_that("Salvere la medesima serie da due sessoioni diverse crea un conflitto", {

})


test_that("I names on an empty Graph return an empy array", {
  g <- GrafoDB("test")
  expect_equal(length(names(g)), 0)
})
