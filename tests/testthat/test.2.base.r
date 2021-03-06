test_that("Posso istanziare un GrafoDB", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  expect_true(is.grafodb(g))
})


test_that("Posso istanziare un GrafoDB con un tag", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  expect_true(is.grafodb(g))
})


test_that("Posso impostare una timeseries con nome nel GrafoDB", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  expect_true("A" %in% names(g))
  expect_true(stats::is.ts(g[["A"]]))
})


test_that("Posso impostare piu' timeseries con nome nel GrafoDB", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  g["B"] <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  g["C"] <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  expect_true("A" %in% names(g))
  expect_true(stats::is.ts(g[["A"]]))
  expect_true("B" %in% names(g))
  expect_true(stats::is.ts(g[["B"]]))
  expect_true("C" %in% names(g))
  expect_true(stats::is.ts(g[["C"]]))

  expect_true(all(g[["A"]] != g[["B"]]))
  expect_true(all(g[["A"]] != g[["C"]]))
  expect_true(all(g[["B"]] != g[["C"]]))
})


test_that("Posso usare delle formule per definire le serie", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  g["B"] <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }
  expect_equal(g[["A"]] + g[["B"]], g[["C"]])
})


test_that("Posso cercare le serie con metadati", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  g["B"] <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }

  g <- setMeta(g, "A", "key", "value")
  g <- setMeta(g, "B", "key", "value")
  g <- setMeta(g, "C", "key", "value1")

  res <- lookup(g, "key", "value")
  expect_equal(length(res), 2)
  expect_true(all(c("A", "B") %in% res))
})


test_that("Posso salvare il grafo sul database", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  g["B"] <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }

  g <- setMeta(g, "A", "key", "value")
  g <- setMeta(g, "B", "key", "value")
  g <- setMeta(g, "C", "key", "value1")

  g1 <- saveGraph(g, "test1")

  expect_true(all(c("A", "B", "C") %in% names(g1)))
})

test_that("names su un grafo vuoto torna un array vuoto", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  expect_equal(length(names(g)), 0)
})


test_that("subset with datasets", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  g["B"] <- stats::ts(runif(10), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C = A + B # nolint
  }

  g <- setMeta(g, "A", "key", "value")
  g <- setMeta(g, "B", "key", "value")
  g <- setMeta(g, "C", "key", "value1")

  expect_true(all(c("A", "B", "C") %in% names(g)))

  ds <- rdataset::Dataset()
  ds["A"] <- stats::ts(c(-1, -2, -3), start = c(1990, 1), frequency = 4)
  ds["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)

  g[names(ds)] <- ds

  expect_equal(g[["A"]], ds[["A"]])
  expect_equal(g[["B"]], ds[["B"]])
})


test_that("a tag with 'p' returns the tag with the ordinal", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  expect_equal(g@ordinal, 0)
  expect_equal(g@tag, "test")

  g <- GrafoDB("testp1")
  expect_equal(g@ordinal, 1)
  expect_equal(g@tag, "testp1")
})


test_that("Cascade subsetting works", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  expect_true("A" %in% names(g))
  expect_true("B" %in% names(g))
})


test_that("Saving preserves network and nodes", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }
  ## ora salvo, e vediamo cosa succede
  g <- saveGraph(g)

  expect_true("A" %in% names(g))
  expect_true("B" %in% names(g))
  expect_true("A" %in% upgrf(g, "C", livello = 1))
  expect_true("B" %in% upgrf(g, "C", livello = 1))
})


test_that("navigate without arguments returns a downgrf", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })

  g <- GrafoDB("test")
  g["A"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  g["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }

  x <- navigate(g)
  expect_true(!is.null(x))
  expect_equal(x[3], "C")
  expect_equal(names(g), x)
})


test_that("posso rimuovere archi", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A) { # nolint
    C <- A # nolint
  }
  expect_true("A" %in% upgrf(g, "C"))
  expect_true(!"B" %in% upgrf(g, "C"))
  expect_equal(length(upgrf(g, "C")), 1)

  g["C"] <- function(B) { # nolint
    C <- B # nolint
  }

  expect_true(!"A" %in% upgrf(g, "C"))
  expect_true("B" %in% upgrf(g, "C"))
  expect_equal(length(upgrf(g, "C")), 1)

  ## ora salvo, e vediamo cosa succede
  g <- saveGraph(g)

  expect_true("B" %in% names(g))
  expect_true("C" %in% names(g))
  expect_true("A" %in% names(g))

  g["C"] <- function(A) { # nolint
    C <- A # nolint
  }

  expect_true("A" %in% upgrf(g, "C"))
  expect_true(!"B" %in% upgrf(g, "C"))
  expect_equal(length(upgrf(g, "C")), 1)
})


test_that("I can cast a empty GrafoDB to a Dataset", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  d <- rdataset::as.dataset(g)
  expect_true(rdataset::is.dataset(d))
  expect_true(all(names(d) %in% names(g)))
  for (name in names(d)) {
    d1 <- d[[name]]
    g1 <- g[[name]]
    expect_equal(d1, g1)
  }
})


test_that("I can cast a GrafoDB to a Dataset", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A) { # nolint
    C <- A # nolint
  }
  d <- rdataset::as.dataset(g)
  expect_true(rdataset::is.dataset(d))
  expect_true(all(c("A", "B", "C") %in% names(d)))
})


test_that("Posso passare non timeseries", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  if (!suppressWarnings(require(tis))) {
    skip("tis::mergeSeries is necessary for this test")
  }
  g <- GrafoDB("test")
  g["A"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  g["B"] <- stats::ts(c(1, 1, 1), start = c(1990, 1), frequency = 4)
  g["periodo"] <- c(1990, 2)
  g["C"] <- function(A, B, periodo) { # nolint
    C <- mergeSeries(A, window(B, start = periodo)) # nolint
  }
  expect_equal(g[["periodo"]], c(1990, 2))
})


test_that("posso salvare non timeseries", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  if (!suppressWarnings(require(tis))) {
    skip("tis::mergeSeries is necessary for this test")
  }
  g <- GrafoDB("test")
  g["A"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  g["B"] <- stats::ts(c(1, 1, 1), start = c(1990, 1), frequency = 4)
  g["periodo"] <- c(1990, 2)
  g["C"] <- function(A, B, periodo) { # nolint
    C <- as.ts(mergeSeries(A, window(B, start = periodo))) # nolint
  }
  g <- saveGraph(g)
  expect_equal(g[["periodo"]], c(1990, 2))
})


test_that("posso memorizzare stringhe", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }
  g["archivio"] <- "cippalippa"
  g <- saveGraph(g)

  g1 <- GrafoDB("test")
  expect_equal(g1[["archivio"]], "cippalippa")
})


test_that("isLeaf torna true per serie foglia", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }
  expect_true(isLeaf(g, "C"))
})


test_that("isLeaf torna false per una serie non foglia", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }
  expect_true(!isLeaf(g, "A"))
  expect_true(!isLeaf(g, "B"))
})


test_that("isRoot torna false per serie foglia", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }
  expect_true(!isRoot(g, "C"))
})


test_that("isRoot torna true per una serie non foglia", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }
  expect_true(isRoot(g, "A"))
  expect_true(isRoot(g, "B"))
})


test_that("Posso subsettare con il $ (dollaro)", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }
  g["D"] <- function(A, C) { # nolint
    D <- A + C # nolint
  }
  d <- g$D

  expect_equal(g$D, g[["D"]])
  expect_equal(g$A, g[["A"]])
  expect_equal(g$C, g[["C"]])
})


test_that("Posso subsettare una singola serie come Dataset", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }

  g["D"] <- function(A, C) { # nolint
    D <- A + C # nolint
  }

  expect_true(rdataset::is.dataset(g["A"]))
  expect_equal(names(g["A"]), "A")
  expect_equal(g[["A"]], g["A"][["A"]])
})


test_that("I get an error if I try to subset a series with missing deps", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  expect_warning({
    g["C"] <- function(A, B, D) { # nolint
      C <- A + B # nolint
    }
  })
})


test_that("isRoot returns true if node is a root", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }
  expect_true(isRoot(g, "A"))
  expect_true(isRoot(g, "B"))
  expect_false(isRoot(g, "C"))
  expect_true(isLeaf(g, "C"))
  expect_false(isLeaf(g, "A"))
  expect_false(isLeaf(g, "B"))
})


test_that("show produces output", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }
  expect_message(show(g))
})


test_that("I can subtract two GrafoDB", {
  for (tag in rilasci("test")$tag) delete_graph(tag)
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 4)
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }

  g["D"] <- 0

  g1 <- GrafoDB("test1")
  g1["A"] <- g1["B"] <- stats::ts(c(1, 1, 1), start = c(1990, 1), frequency = 4)
  g1["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }

  expect_warning(diff <- g1 - g)
  expect_true(rdataset::is.dataset(diff))
  expect_true(all(names(diff) %in% c("A", "B", "C")))
  expect_true(!"D" %in% names(diff))
  for (name in names(diff)) {
    expect_equal(diff[[name]], g1[[name]] - g[[name]])
  }
})

for (tag in rilasci("test")$tag) delete_graph(tag)

test_that("I can't subtract container with different types of objects", {
  for (tag in rilasci("test")$tag) delete_graph(tag)
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })

  g <- GrafoDB("test")
  g["A"] <- g["B"] <- 0
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }

  g1 <- GrafoDB("test1")
  g1["A"] <- g1["B"] <- stats::ts(c(1, 1, 1), start = c(1990, 1), frequency = 4)
  g1["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }

  expect_error(diff <- g - g1, "Different object classes")
})


test_that("expr returns a list of formulas with multiple names", {
  for (tag in rilasci("test")$tag) delete_graph(tag)
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })

  g <- GrafoDB("test")
  g["A"] <- g["B"] <- 0
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }

  g["D"] <- function(A, C) { # nolint
    D <- A + C # nolint
  }

  expect_type(expr(g, c("C", "D")), "list")
  ll <- expr(g, c("C", "D"))
  expect_true(all(names(ll) %in% c("C", "D")))
})


test_that("ser fails if returned object is not a timeseries", {
  for (tag in rilasci("test")$tag) delete_graph(tag)
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })

  g <- GrafoDB("test")
  g["A"] <- g["B"] <- 0
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }

  g["D"] <- function(A, C) { # nolint
    D <- A + C # nolint
  }
  expect_error(ser(g, "C"), "non e' un oggetto ts")
})


test_that("ser in debug fails if series has no formula", {
  for (tag in rilasci("test")$tag) delete_graph(tag)
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })

  g <- GrafoDB("test")
  g["A"] <- g["B"] <- 0
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }

  expect_error(ser(g, "A", debug = TRUE), "non e' una serie con formula")
})


test_that("ser in debug fails if name doesn't exist", {
  for (tag in rilasci("test")$tag) delete_graph(tag)
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })

  g <- GrafoDB("test")
  g["A"] <- g["B"] <- 0
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }

  expect_error(ser(g, "NONESISTO", debug = TRUE),
    "non e' una serie del grafo")
})


test_that("an empty graph returns no names", {
  for (tag in rilasci("test")$tag) delete_graph(tag)
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })

  g <- GrafoDB("test")

  expect_type(names(g), "character")
  expect_equal(names(g), character(0))
})


test_that("evaluate raises an error on unknown name series", {
  on.exit(delete_graph("test"))
  g <- GrafoDB("test")
  expect_error(evaluate(g, "NONESISTO"))
})

test_that("I can evaluate multiple series on a single evaluate call", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })
  g <- GrafoDB("test")
  g["A"] <- g["B"] <- 0
  g["C"] <- function(A, B) { # nolint
    C <- A + B # nolint
  }

  g["D"] <- function(C, B) { # nolint
    D <- C - B # nolint
  }

  g <- saveGraph(g) # to clean internal structures
  g <- evaluate(g, c("D", "C")) # just for codecov
  expect_equal(hash::keys(g@functions), character(0))
  expect_equal(hash::keys(g@data), c("C", "D"))
})


test_that("Se imposto una serie per formula senza definirla ho un errore", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })

  g <- GrafoDB("test")
  g["A"] <- g["B"] <- 0
  expect_error({
    g["E"] <- function(A, B) { # nolint
      D <- A + B # nolint
    }
  })
})


test_that("Posso usare funzioni complesse nelle formule", {
  on.exit({
    for (tag in rilasci("test")$tag) delete_graph(tag)
  })

  if (!requireNamespace("tempdisagg")) {
    skip("Tempdisagg not installed")
  }

  g <- GrafoDB("test")
  g["A"] <- g["B"] <- stats::ts(runif(100), start = c(1995, 1), frequency = 4)
  expect_error({g["C"] <- function(A, B) { # nolint
    if (!is.function(tempdisagg::td)) {
      fail("td not found")
    }
    C <- A + B # nolint
  }}, NA) # this means expect no error

  expect_s3_class(g[["C"]], "ts")
})


test_that("Posso valutare funzioni passate come stringhe", {
  g <- GrafoDB("test")
  on.exit({
    delete_graph("test")
  })

  g["A"] <- 1
  g["B"] <- "function(A) { B = A}"

  expect_equal(g[["B"]], g[["A"]])
})


test_that("Posso valutare funzioni passate come stringhe", {
  g <- GrafoDB("test")
  on.exit({
    delete_graph("test")
  })

  g["A"] <- 1
  g["B"] <- "function() { B = 2 }"

  expect_equal(g[["B"]], 2)
})


test_that("Posso valutare espressioni come stringhe con newlines", {

  g <- GrafoDB("test")
  on.exit({
    delete_graph("test")
  })

  g["A"] <- 1
  g["B"] <- "function(A) { C = 2\n B=C+A }"
  expect_equal(g[["B"]], 3)
})
