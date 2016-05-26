context("Concorrenza")

elimina("test")
elimina("test1")

test_that("Salvare la medesima serie da due sessioni diverse crea un conflitto", {
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
  old_timestamp <- g1@timestamp
  g2 <- GrafoDB("test1")
  g1["A"] <- newA1 <- TSERIES(rep(1,10), START=c(1990,1), FREQ=4)

  g1 <- saveGraph(g1)

  expect_true(g1@timestamp > old_timestamp)
  expect_true(all(g1[["A"]] == newA1))
  
  g2["A"] <- newA2 <- TSERIES(rep(0,10), START=c(1990,1), FREQ=4)
  expect_warning(saveGraph(g2), "Ci sono conflitti")
  expect_true(hasConflicts(g1))
  expect_true(hasConflicts(g2))

  g <- GrafoDB("test1")
  
  expect_true(all(g[["A"]] ==  newA1))
  expect_true(!identical(g[["A"]], newA2))
})

elimina("test")
elimina("test1")

test_that("Salvare lo stesso grafo con interventi su serie distinte non crea conflitti", {
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
  g2 <- GrafoDB("test1")
  newA <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  newB <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g1["A"] <- newA
  g2["B"] <- newB
  saveGraph(g1, "test1")
  saveGraph(g2, "test1")

  g <- GrafoDB("test1")
  expect_true(all(g[["A"]] == newA))
  expect_true(all(g[["B"]] == newB))
})

elimina("test")
elimina("test1")

test_that("Salvare lo stesso grafo con formula aggiunta", {
  g <- GrafoDB("test")
  g["A"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["B"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B    
  }

  saveGraph(g, "test1")
  g1 <- GrafoDB("test1")
  g2 <- GrafoDB("test1")

  g1["D"] <- function(A, C) {
    D = A * C
  }

  g2["E"] <- function(A, C) {
    E = A - C
  }

  saveGraph(g1)
  saveGraph(g2)
  
  g <- GrafoDB("test1")

  expect_true("D" %in% names(g))
  expect_true("E" %in% names(g))
  expect_true(!hasConflicts(g))
})

elimina("test")
elimina("test1")

test_that("Salvare lo stesso grafo con formula in conflitto", {
  g <- GrafoDB("test")
  g["A"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["B"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B    
  }
  saveGraph(g, "test1")
  
  g1 <- GrafoDB("test1")
  g2 <- GrafoDB("test1")

  g1["D"] <- function(A, C) {
    D = A * C
  }

  g2["D"] <- function(A, C) {
    D = A - C
  }

  g1 <- saveGraph(g1)
  Sys.sleep(1)
  ## in seguito alla non necessita' di dare saveGraph(g1), che per usabilita' e' stato aggiunto
  ## il side-effect di cambiare g1 nel env del parent.frame, il seguente warning non uscira' mai
  ## expect_warning(saveGraph(g2), "Ci sono conflitti sugli archi")
  
  expect_warning(saveGraph(g2), "Ci sono conflitti")

  g <- GrafoDB("test1")
  expect_true(hasConflicts(g))  
})

elimina("test")
elimina("test1")

test_that("Tra i conflitti viene segnalata solo le serie modificate, non le serie figlie", {
  g <- GrafoDB("test")
  g["A"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["B"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B    
  }
  g["D"] <- function(C) {
    D = 2 * C
  }
  
  g <- saveGraph(g, "test1")
  
  g1 <- GrafoDB("test1")
  g2 <- GrafoDB("test1")

  g1["C"] <- function(A) {
    C = A/2
  }

  g2["C"] <- function(A) {
    C = A*2
  }
  g1 <- saveGraph(g1)
  expect_warning(saveGraph(g2))
  conflicts <- getConflicts(g1)
  lista_nomi <- as.character(conflicts$name)
  expect_true("C" %in% lista_nomi)
  expect_true(!"D" %in% lista_nomi)
})

elimina("test")
elimina("test1")
