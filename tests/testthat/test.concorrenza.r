context("Concurrency")

identicalts <- function(x, y, toll=0.000001) {
  all(abs(x-y) < toll)
}

setup <- function(tag) {
  g <- GrafoDB(tag)
  g["A"] <- ts(runif(10), start=c(1990,1), frequency=4)
  g["B"] <- ts(runif(10), start=c(1990,1), frequency=4)
  g["C"] <- function(A, B) {
    C = A + B
  }
  g
}

test_that("Salvare una serie non crea un conflitto", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- setup("test")
  g <- saveGraph(g, "test", msg="test")
  
  g1 <- GrafoDB("test")
  g2 <- GrafoDB("test")

  expect_equal(g1@timestamp, g2@timestamp)

  g1["A"] <- newA1 <- ts(rep(1,10), start=c(1990,1), frequency=4)
  #Sys.sleep(.5)
  g1 <- saveGraph(g1, msg="test")

  expect_true(g1@timestamp != g2@timestamp)
  expect_true(identicalts(g1[["A"]], newA1))
})

test_that("Salvare la stessa serie in due sessioni differenti crea un conflitto", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- setup("test")
  g <- saveGraph(g, "test")
  g1 <- GrafoDB("test")
  g2 <- GrafoDB("test")


  expect_equal(g1@timestamp, g2@timestamp)

  g1["A"] <- newA1 <- ts(rep(0,10), start=c(1990,1), frequency=4)
  g2["A"] <- newA2 <- ts(rep(1,10), start=c(1990,1), frequency=4)
  g1 <- saveGraph(g1)
  # Sys.sleep(2)
  with_mock(
    'GrafoDB:::get_outer_data_names' = function(...) "A", {
      expect_warning(saveGraph(g2), "Ci sono conflitti")

      ## sia g1 che g2, in quanto "test1" devono riportare dei conflitti
      expect_true(has_conflicts(g1))
      expect_true(has_conflicts(g2))

      g <- GrafoDB("test")

      expect_true(all(abs(g[["A"]]-newA1) < 0.0000001))
      expect_true(any(abs(g[["A"]]-newA2) > 0.0000001))

      conflicts <- getDataConflicts(g)
      expect_is(conflicts, "list")
      expect_equal(names(conflicts), "A")
    })
})

for (tag in rilasci("test")$tag) delete_graph(tag)

test_that("Salvare lo stesso grafo con interventi su serie distinte non crea conflitti", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- setup("test")
  g <-saveGraph(g, "test", msg="test")

  g1 <- GrafoDB("test")
  g2 <- GrafoDB("test")

  newA <- ts(runif(10), start=c(1990,1), frequency=4)
  newB <- ts(runif(10), start=c(1990,1), frequency=4)

  g1["A"] <- newA
  g2["B"] <- newB

  g1 <- saveGraph(g1, "test", msg="test")
  g2 <- saveGraph(g2, "test", msg="test")

  g <- GrafoDB("test")
  expect_true(identicalts(g[["A"]], newA))
  expect_true(identicalts(g[["B"]], newB))
})

test_that("Salvare lo stesso grafo con formula aggiunta", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- setup("test")
  g <- saveGraph(g, "test", msg="test")

  g1 <- GrafoDB("test")
  g2 <- GrafoDB("test")

  g1["D"] <- function(A, C) {
    D = A * C
  }

  g2["E"] <- function(A, C) {
    E = A - C
  }

  g1 <- saveGraph(g1, msg="test")
  g2 <- saveGraph(g2, msg="test")
  
  g <- GrafoDB("test")

  expect_true("D" %in% names(g))
  expect_true("E" %in% names(g))
  expect_true(!has_conflicts(g))
})

for (tag in rilasci("test")$tag) delete_graph(tag)

test_that("Salvare lo stesso grafo con formula in conflitto", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- setup("test")
  g <- saveGraph(g, "test", msg="test")
  
  g1 <- GrafoDB("test")
  g2 <- GrafoDB("test")

  g1["D"] <- function(A, C) {
    D <- A * C
  }

  g2["D"] <- function(A, C) {
    D <- A - C
  }

  g1 <- saveGraph(g1, msg="test")
  Sys.sleep(.2)
  ## in seguito alla non necessita' di dare saveGraph(g1), che per usabilita' e' stato aggiunto
  ## il side-effect di cambiare g1 nel env del parent.frame, il seguente warning non uscira' mai
  ## expect_warning(saveGraph(g2), "Ci sono conflitti sugli archi")
  with_mock(
    'GrafoDB:::get_outer_formula_names' = function(...) "D", {
      g2 <- expect_warning(saveGraph(g2, msg="test"), "Ci sono conflitti")
      g <- GrafoDB("test")
      expect_true(has_conflicts(g))
    })
})

test_that("Tra i conflitti viene segnalata solo le serie modificate, non le serie figlie", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- setup("test")

  g["D"] <- function(C) {
    D = 2 * C
  }
  
  g <- saveGraph(g, "test", msg="test")
  
  g1 <- GrafoDB("test")
  g2 <- GrafoDB("test")

  g1["C"] <- function(A) {
    C = A/2
  }

  g2["C"] <- function(A) {
    C = A*2
  }
  g1 <- saveGraph(g1)
  Sys.sleep(1)
  with_mock(
    'GrafoDB:::get_outer_formula_names' = function(...) "C", {
      g2 <- expect_warning(saveGraph(g2))
      conflicts <- getConflicts(g1)
      lista_nomi <- as.character(conflicts$name)
      expect_true("C" %in% lista_nomi)
      expect_true(!"D" %in% lista_nomi)
      
      expect_equal(getConflicts(g1, "C")$name, "C")
      expect_equal(nrow(getConflicts(g1, "C")), 1)
      conflict <- getFormulaConflicts(g)
      expect_is(conflict, "data.frame")
      conflictC <- getFormulaConflicts(g, "C")
      expect_is(conflictC, "data.frame")
      expect_equal(nrow(conflictC), 1)
    })
})

for (tag in rilasci("test")$tag) delete_graph(tag)

test_that(
  paste(
    "Cambiamenti nel grafo in due sessioni diverse:",
    "differenti dati vengono incluse al salvataggio"),
  {
    on.exit({
      for (tag in rilasci("test")$tag) delete_graph(tag)
    })
    g <- GrafoDB("test")
    g["A"] <- 1
    g["B"] <- 2
    g["C"] <- function(A, B) {
      C <- A + B + 1
    }
    
    g["D"] <- function(C) {
      D <- C
    }
    
    expect_equal(g[["C"]] , g[["D"]])
    
    g <- saveGraph(g)
    
    g1 <- GrafoDB("test")
    g2 <- GrafoDB("test")
    
    ## let's see if the graph are roughly the same
    expect_equal(names(g1), names(g2))
    for (name in names(g1)) {
      expect_equal(g1[[name]], g2[[name]])
    }
    
    expect_identical(igraph::get.edgelist(g1@network), igraph::get.edgelist(g2@network))
    ## ok, they are identical
    
    ## not let's change C in g1 and save it
    g1["C"] = function(A, B) {
      C <- A + B
    }
    
    g1 <- saveGraph(g1)
    
    expect_true(g1[["C"]] != g2[["C"]])
    expect_true(g1[["D"]] != g2[["D"]])
    expect_true(expr(g1, "C") != expr(g2, "C"))
    
    g2 <- saveGraph(g2)
    expect_equal(g1[["C"]], g2[["C"]])
    expect_equal(g1[["D"]], g2[["D"]])
    expect_equal(expr(g1, "C"), expr(g2, "C"))  
})

test_that("fixConflicts removes conflicts", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- 1
  g["B"] <- 2
  g["C"] <- function(A, B) {
    C <- A + B
  }
  saveGraph(g)

  g1 <- GrafoDB("test")
  g2 <- GrafoDB("test")

  expect_equal(g1@timestamp, g2@timestamp)
  g1["A"] <- 0
  # non mi aspetto warnings...
  g1 <- expect_warning(saveGraph(g1), NA)
  g2["A"] <- 2
  expect_true(g2@timestamp != g1@timestamp)

  g2<- expect_warning(saveGraph(g2))
  expect_equal(has_conflicts(g1), has_conflicts(g2))
  expect_true(has_conflicts(g1))
  expect_true(has_conflicts(g2))

  fixConflicts(g1)
  # non mi aspetto piu' conflitti
  expect_error(saveGraph(g2), NA)
  expect_equal(has_conflicts(g1), has_conflicts(g2))
  expect_false(has_conflicts(g1))
  expect_false(has_conflicts(g2))
})

test_that("resync gets called when two sessions updates a Graph", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] = 1
  g["B"] = 0
  g["C"] <- function(A, B) {
    C <- A + B
  }
  g <- saveGraph(g)

  g1 <- GrafoDB("test")
  g2 <- GrafoDB("test")

  g1["A"] <- 0
  expect_equal(g1[["C"]], 0)
  expect_equal(g2[["C"]], 1)
  g1 <- saveGraph(g1)
  Sys.sleep(.01)
  expect_true(need_resync(g2))
  g2 <- saveGraph(g2)
  # expect_message(saveGraph(g2), "Resync started")
  expect_equal(g2[["C"]],  0)
})


test_that("La rilevazione di conflitti su serie primitive con missing non crea errori", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })

  g <- setup("test")
  g <- saveGraph(g)

  g1 <- GrafoDB("test")
  g2 <- GrafoDB("test")

  # accertati che siano lo stesso grafo
  expect_equal(g1@timestamp, g2@timestamp)
  expect_equal(g1@tag, g2@tag)
  
  g1["A"] <- ts(rep(1, 10), start=c(1990,1), frequency=4)
  g1 <- saveGraph(g1)
  Sys.sleep(.01)
  g2["A"] <- ts(rep(10, 10), start=c(1990,1), frequency=4)
  
  expect_true(g1@timestamp != g2@timestamp)
  expect_true(g1@timestamp > g2@timestamp)
  expect_equal(g1@tag, g2@tag)
  expect_warning(expect_error(saveGraph(g2), NA), "Ci sono conflitti")
  
  expect_true(has_conflicts(g2))
  
})
