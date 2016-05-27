context("Concorrenza")

dbSettings(TRUE)
elimina("test")
elimina("test1")


setup <- function(tag) {
  g <- GrafoDB(tag)
  g["A"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["B"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["C"] <- function(A, B) {
    C = A + B
  }
  g
}

test_that("Salvare una serie non crea un conflitto", {
  g <- setup("test")
  saveGraph(g, "test1", msg="test")
  
  g1 <- GrafoDB("test1")
  g2 <- GrafoDB("test1")

  expect_equal(g1@timestamp, g2@timestamp)
  
  g1["A"] <- newA1 <- TSERIES(rep(1,10), START=c(1990,1), FREQ=4)
  Sys.sleep(.5)
  g1 <- saveGraph(g1, msg="test")

  expect_true(g1@timestamp != g2@timestamp)
  expect_true(all(g1[["A"]] == newA1))
})

for(tag in rilasci("test")$tag) elimina(tag)

test_that("Salvare la stessa serie in due sessioni differenti crea un conflitto", {
  g <- setup("test")
  saveGraph(g, "test1", msg="test")
  g1 <- GrafoDB("test1")
  g2 <- GrafoDB("test1")

  
  expect_equal(g1@timestamp, g2@timestamp)
  
  g1["A"] <- newA1 <- TSERIES(rep(0,10), START=c(1990,1), FREQ=4)
  g2["A"] <- newA2 <- TSERIES(rep(1,10), START=c(1990,1), FREQ=4)
  saveGraph(g1, msg="test")
  Sys.sleep(2)
  expect_warning(saveGraph(g2, msg="test"), "Ci sono conflitti")

  ## sia g1 che g2, in quanto "test1" devono riportare dei conflitti
  expect_true(hasConflicts(g1))
  expect_true(hasConflicts(g2))

  g <- GrafoDB("test1")
  
  expect_true(all(g[["A"]] ==  newA1))
  expect_true(!any(g[["A"]] ==  newA2))
})

for(tag in rilasci("test")$tag) elimina(tag)

test_that("Salvare lo stesso grafo con interventi su serie distinte non crea conflitti", {
  g <- setup("test")
  saveGraph(g, "test1", msg="test")

  g1 <- GrafoDB("test1")
  g2 <- GrafoDB("test1")

  newA <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  newB <- TSERIES(runif(10), START=c(1990,1), FREQ=4)

  g1["A"] <- newA
  g2["B"] <- newB

  saveGraph(g1, "test1", msg="test")
  saveGraph(g2, "test1", msg="test")

  g <- GrafoDB("test1")
  expect_true(all(g[["A"]] == newA))
  expect_true(all(g[["B"]] == newB))
})

for(tag in rilasci("test")$tag) elimina(tag)

test_that("Salvare lo stesso grafo con formula aggiunta", {
  g <- setup("test")
  saveGraph(g, "test1", msg="test")

  g1 <- GrafoDB("test1")
  g2 <- GrafoDB("test1")

  g1["D"] <- function(A, C) {
    D = A * C
  }

  g2["E"] <- function(A, C) {
    E = A - C
  }

  saveGraph(g1, msg="test")
  saveGraph(g2, msg="test")
  
  g <- GrafoDB("test1")

  expect_true("D" %in% names(g))
  expect_true("E" %in% names(g))
  expect_true(!hasConflicts(g))
})

for(tag in rilasci("test")$tag) elimina(tag)

test_that("Salvare lo stesso grafo con formula in conflitto", {
  g <- setup("test")
  saveGraph(g, "test1", msg="test")
  
  g1 <- GrafoDB("test1")
  g2 <- GrafoDB("test1")

  g1["D"] <- function(A, C) {
    D = A * C
  }

  g2["D"] <- function(A, C) {
    D = A - C
  }

  g1 <- saveGraph(g1, msg="test")
  Sys.sleep(.5)
  ## in seguito alla non necessita' di dare saveGraph(g1), che per usabilita' e' stato aggiunto
  ## il side-effect di cambiare g1 nel env del parent.frame, il seguente warning non uscira' mai
  ## expect_warning(saveGraph(g2), "Ci sono conflitti sugli archi")
  
  expect_warning(saveGraph(g2, msg="test"), "Ci sono conflitti")

  g <- GrafoDB("test1")
  expect_true(hasConflicts(g))  
})

for(tag in rilasci("test")$tag) elimina(tag)

test_that("Tra i conflitti viene segnalata solo le serie modificate, non le serie figlie", {
  g <- setup("test")

  g["D"] <- function(C) {
    D = 2 * C
  }
  
  g <- saveGraph(g, "test1", msg="test")
  
  g1 <- GrafoDB("test1")
  g2 <- GrafoDB("test1")

  g1["C"] <- function(A) {
    C = A/2
  }

  g2["C"] <- function(A) {
    C = A*2
  }
  g1 <- saveGraph(g1)
  Sys.sleep(1)
  expect_warning(saveGraph(g2))
  conflicts <- getConflicts(g1)
  lista_nomi <- as.character(conflicts$name)
  expect_true("C" %in% lista_nomi)
  expect_true(!"D" %in% lista_nomi)
})

for(tag in rilasci("test")$tag) elimina(tag)
