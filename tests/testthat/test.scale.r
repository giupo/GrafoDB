context("Scalability tests")

tt <- stats::ts(rep(1, 5), start = c(1990, 1), frequency = 4)

proxytest <- function(numero) {
  g <- GrafoDB("test")
  deps <- paste0("A", seq(numero))
  formula_b <- paste0("B = ", paste0(deps, collapse = " + "))
  g@functions["B"] <- formula_b
  for (nome in deps) {
    g[nome] <- tt
  }
  g@network <- igraph::graph.data.frame(as.data.frame(cbind(deps, "B")))
  g <- evaluate(g, "B")
  expect_true("B" %in% names(g))
  expected <- g[["B"]]
  expect_true(stats::is.ts(expected))
  expect_equal(expected[1], numero)
  ser(g, "B")
  delete_graph("test")
}

test_that("A single series can have > 100 deps", {
  proxytest(101)
})

delete_graph("test")

test_that("A single series can have > 200 deps", {
  proxytest(201)
})
delete_graph("test")


test_that("A single series can have > 300 deps", {
  proxytest(301)
})

delete_graph("test")
