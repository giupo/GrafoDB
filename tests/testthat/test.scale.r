context("Scalability tests")

tt <- TSERIES(c(1,1,1,1,1), START=c(1990,1), FREQ=4)

proxytest <- function(numero) {
  g <- GrafoDB("test")
  numero_padri <- 201
  deps <- paste0("A", seq(numero))
  formula_B <- paste0("B = ", paste0(deps, collapse=" + "))
  g@functions["B"] <- formula_B
  for(nome in deps) {
    g[nome] <- tt
  }
  g@network <- graph.data.frame(as.data.frame(cbind(deps, "B")))
  g <- evaluate(g, "B")
  expect_true("B" %in% names(g))
  expected <- g[["B"]]
  expect_true(is.bimets(expected))  
  expect_equal(expected[1], numero)
  ser(g, "B")  
  elimina("test")
}

test_that("A single series can have > 100 deps", {
  proxytest(101)
})
elimina("test")


test_that("A single series can have > 200 deps", {
  proxytest(201)
})
elimina("test")


test_that("A single series can have > 300 deps", {
  proxytest(301)
})

elimina("test")
