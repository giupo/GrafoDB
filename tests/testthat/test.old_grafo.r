context("Old API")


setup <- function(tag) {
  g <- GrafoDB(tag)
  g["A"] <- ts(runif(10), start=c(1990,1), frequency=4)
  g["B"] <- ts(runif(10), start=c(1990,1), frequency=4)
  g["C"] <- function(A, B) {
    C = A + B
  }
  g["D"] <- function() {
    D <- ts(runif(10), start=c(1990,1), frequency=4)
  }

  setMeta(g, "A", "KEYA", "VALUE")
  setMeta(g, "B", "KEYA", "VALUE")
  
  g
  
}

test_that("isPrimitive returns true if a series is primitive (only numbers, no function)", {
  on.exit({
    delete_graph("test")
  })
  g <- setup("test")
  expect_true(isPrimitive(g, "A"))
  expect_true(isPrimitive(g, "B"))
  expect_false(isPrimitive(g, "C"))
  expect_false(isPrimitive(g, "D"))
})

test_that("isElementary returns true if a series is elementary (numbers and formula, no parents)", {
  on.exit({
    delete_graph("test")
  })
  g <- setup("test")
  expect_false(isElementary(g, "A"))
  expect_false(isElementary(g, "B"))
  expect_false(isElementary(g, "C"))
  expect_true(isElementary(g, "D"))
})


test_that("isAggregate returns true if a series is elementary (numbers and formula and parents)", {
  on.exit({
    delete_graph("test")
  })
  g <- setup("test")
  expect_false(isAggregate(g, "A"))
  expect_false(isAggregate(g, "B"))
  expect_true(isAggregate(g, "C"))
  expect_false(isAggregate(g, "D"))
})

test_that("listNodes returns all the names of the Graph", {
  on.exit({
    delete_graph("test")
  })
  g <- setup("test")
  expect_equal(listNodes(g), names(g))
  expect_true(all(listNodes(g) %in% c("A", "B", "C", "D")))
})

test_that("getDependencies return parents of an aggregate", {
  on.exit({
    delete_graph("test")
  })
  g <- setup("test")
  expect_equal(getDependencies(g, "C"), deps(g, "C"))
  expect_true(all(getDependencies(g, "C") %in% c("A", "B")))
})

test_that("getTask returns the formula of the aggregate", {
  on.exit({
    delete_graph("test")
  })
  g <- setup("test")

  expect_equal(getTask(g, "C"), expr(g, "C"))
  expect_equal(getTask(g, "C"), "C = A + B")
  expect_equal(getTask(g, "A"), expr(g, "A"))  
})

test_that("searchNode returns a list of nodes matching search criteria", {
  on.exit({
    delete_graph("test")
  })
  g <- setup("test")

  expect_equal(searchNode(g, "KEYA", "VALUE"), lookup(g, "KEYA", "VALUE"))
  expect_equal(searchNode(g, "KEYA", "VALUE"), c("A", "B"))
  expect_error(searchNode(g, "NONESISTO", "NONESISTO"))
  expect_equal(lookup(g, "NONESISTO", "NONESISTO"), character(0))
})

test_that("describe is equal to navigate", {
  on.exit({
    delete_graph("test")
  })
  g <- setup("test")

  expect_equal(describe(g, "A"), downgrf(g, "A"))
  expect_equal(describe(g, "C", mode="in", order=1), upgrf(g, "C"))
})
