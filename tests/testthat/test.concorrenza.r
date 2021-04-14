context("Concurrency")

setup <- function(tag) {
  g <- GrafoDB(tag)
  g["A"] <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  g["B"] <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C = A + B # nolint
  }
  g
}

test_that("Salvare una serie non crea un conflitto", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- setup("test")
  g <- saveGraph(g, "test", msg = "test")

  g1 <- GrafoDB("test")
  g2 <- GrafoDB("test")

  expect_equal(g1@timestamp, g2@timestamp)

  g1["A"] <- new_a_1 <- stats::ts(rep(1, 10), start = c(1990, 1), frequency = 4)
  g1 <- saveGraph(g1, msg = "test")

  expect_true(g1@timestamp != g2@timestamp)
  expect_true(identicalts(g1[["A"]], new_a_1))
})

test_that("Save same object in two distinct sessions creates a conflict", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- setup("test")
  g <- saveGraph(g, "test")
  g1 <- GrafoDB("test")
  g2 <- GrafoDB("test")

  expect_equal(g1@timestamp, g2@timestamp)

  g1["A"] <- new_a_1 <- stats::ts(rep(0, 10), start = c(1990, 1), frequency = 4)
  g2["A"] <- new_a_2 <- stats::ts(rep(1, 10), start = c(1990, 1), frequency = 4)
  g1 <- saveGraph(g1)

  with_mock(
    "GrafoDB:::get_outer_data_names" = function(...) "A", {
      expect_warning(saveGraph(g2), "Ci sono conflitti")

      ## sia g1 che g2, in quanto "test1" devono riportare dei conflitti
      expect_true(has_conflicts(g1))
      expect_true(has_conflicts(g2))

      g <- GrafoDB("test")

      expect_true(all(abs(g[["A"]] - new_a_1) < 0.0000001))
      expect_true(any(abs(g[["A"]] - new_a_2) > 0.0000001))

      conflicts <- getDataConflicts(g)
      expect_is(conflicts, "list")
      expect_equal(names(conflicts), "A")
    })
})

for (tag in rilasci("test")$tag) delete_graph(tag)

test_that("save two different objects doesn't create a conflict", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- setup("test")
  g <- saveGraph(g, "test", msg = "test")

  g1 <- GrafoDB("test")
  g2 <- GrafoDB("test")

  new_a <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  new_b <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)

  g1["A"] <- new_a
  g2["B"] <- new_b

  g1 <- saveGraph(g1, "test", msg = "test")
  g2 <- saveGraph(g2, "test", msg = "test")

  g <- GrafoDB("test")
  expect_true(identicalts(g[["A"]], new_a))
  expect_true(identicalts(g[["B"]], new_b))
})

test_that("Salvare lo stesso grafo con formula aggiunta", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- setup("test")
  g <- saveGraph(g, "test", msg = "test")

  g1 <- GrafoDB("test")
  g2 <- GrafoDB("test")

  g1["D"] <- function(A, C) { # nolint
    D = A * C # nolint
  }

  g2["E"] <- function(A, C) { # nolint
    E = A - C # nolint
  }

  g1 <- saveGraph(g1, msg = "test")
  g2 <- saveGraph(g2, msg = "test")

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
  g <- saveGraph(g, "test", msg = "test")

  g1 <- GrafoDB("test")
  g2 <- GrafoDB("test")

  g1["D"] <- function(A, C) { # nolint
    D <- A * C # nolint
  }

  g2["D"] <- function(A, C) { # nolint
    D <- A - C # nolint
  }

  g1 <- saveGraph(g1, msg = "test")

  ## in seguito alla non necessita' di dare saveGraph(g1), che per
  ## usabilita' e' stato aggiunto il side-effect di cambiare g1 nel
  ## env del parent.frame, il seguente warning non uscira' mai

  with_mock(
    "GrafoDB:::get_outer_formula_names" = function(...) "D", {
      g2 <- expect_warning(saveGraph(g2, msg = "test"), "Ci sono conflitti")
      g <- GrafoDB("test")
      expect_true(has_conflicts(g))
    })
})

test_that("in case of conflict, only the root series are reported", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- setup("test")

  g["D"] <- function(C) { # nolint
    D = 2 * C # nolint
  }

  g <- saveGraph(g, "test", msg = "test")

  g1 <- GrafoDB("test")
  g2 <- GrafoDB("test")

  g1["C"] <- function(A) { # nolint
    C = A/2 # nolint
  }

  g2["C"] <- function(A) { # nolint
    C = A * 2 # nolint
  }

  g1 <- saveGraph(g1)
  Sys.sleep(1)
  with_mock(
    "GrafoDB:::get_outer_formula_names" = function(...) "C", {
      g2 <- expect_warning(saveGraph(g2))
      conflicts <- getConflicts(g1)
      lista_nomi <- as.character(conflicts$name)
      expect_true("C" %in% lista_nomi)
      expect_true(!"D" %in% lista_nomi)

      expect_equal(getConflicts(g1, "C")$name, "C")
      expect_equal(nrow(getConflicts(g1, "C")), 1)
      conflict <- getFormulaConflicts(g)
      expect_is(conflict, "data.frame")
      conflict_c <- getFormulaConflicts(g, "C")
      expect_is(conflict_c, "data.frame")
      expect_equal(nrow(conflict_c), 1)
    })
})

for (tag in rilasci("test")$tag) delete_graph(tag)

test_that(
  paste(
    "Cambiamenti nel grafo in due sessioni diverse:",
    "differenti dati vengono incluse al salvataggio"), {
    on.exit({
      for (tag in rilasci("test")$tag) delete_graph(tag)
    })
    g <- GrafoDB("test")
    g["A"] <- 1
    g["B"] <- 2
    g["C"] <- function(A, B) { # nolint
      C <- A + B + 1 # nolint
    }

    g["D"] <- function(C) { # nolint
      D <- C # nolint
    }

    expect_equal(g[["C"]], g[["D"]])

    g <- saveGraph(g)

    g1 <- GrafoDB("test")
    g2 <- GrafoDB("test")

    ## let's see if the graph are roughly the same
    expect_equal(names(g1), names(g2))
    for (name in names(g1)) {
      expect_equal(g1[[name]], g2[[name]])
    }

    expect_identical(igraph::get.edgelist(g1@network),
      igraph::get.edgelist(g2@network))

    ## ok, they are identical

    ## not let's change C in g1 and save it
    g1["C"] <- function(A, B) { # nolint
      C <- A + B # nolint
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
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
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

  g2 <- expect_warning(saveGraph(g2))
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
  g["A"] <- 1
  g["B"] <- 0
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }
  g <- saveGraph(g)

  g1 <- GrafoDB("test")
  g2 <- GrafoDB("test")

  g1["A"] <- 0
  expect_equal(g1[["C"]], 0)
  expect_equal(g2[["C"]], 1)
  g1 <- saveGraph(g1)

  expect_true(need_resync(g2))
  g2 <- saveGraph(g2)

  expect_equal(g2[["C"]],  0)
})


test_that("Series with conflicts with NA don't trigger errors", {
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

  g1["A"] <- stats::ts(rep(1, 10), start = c(1990, 1), frequency = 4)
  g1 <- saveGraph(g1)
  g2["A"] <- stats::ts(rep(10, 10), start = c(1990, 1), frequency = 4)

  expect_true(g1@timestamp != g2@timestamp)
  expect_true(g1@timestamp > g2@timestamp)
  expect_equal(g1@tag, g2@tag)
  expect_warning(expect_error(saveGraph(g2), NA), "Ci sono conflitti")

  expect_true(has_conflicts(g2))

})
