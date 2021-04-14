context("edit functions")

setup <- function(tag) {
  g <- GrafoDB(tag)
  g["A"] <- g["B"] <- ts(c(0,0,0), start = c(1990, 1), frequency = 1)
  g["C"] <- function(A, B) {
    C = (A + 1) * (B + 2)
  }
  g <- saveGraph(g)
  
  setMeta(g, "A", "KEY", "VALUE1")
  setMeta(g, "A", "KEY", "VALUE2")
  setMeta(g, "B", "KEY", "VALUE1")
  g
}


test_that("I can edit a function 1", {
  skip_if_not(require(mockery), "mockery required")
  on.exit(delete_graph("test"))
  g <- setup("test")

  stub(.edita, 'utils::file.edit', function(file, title = title) {
    message(file)
    task <- "C = A / B"
    deps <- c("A", "B")
    name <- "C"
    write(clutter_with_params(task, name, deps), file = file)
  })

  g <- .edita(g, "C")
  expect_equal(g@functions[["C"]], "C = A/B")
})

test_that("I can edit a function 2", {
  skip_if_not(require(mockery), "mockery required")
  on.exit(delete_graph("test"))
  g <- setup("test")

  stub(.edita, 'utils::file.edit', function(file, title=title) {
    task <- "D = A - B"
    deps <- c("A", "B")
    name <- "D"
    write(clutter_with_params(task, name, deps), file = file)
  })
  g <- .edita(g, "D")
  expect_equal(g@functions[["D"]], "D = A - B")
})

test_that("I can replace a function", {
  skip_if_not(require(mockery), "mockery required")
  on.exit(delete_graph("test"))
  g <- setup("test")

  stub(.edita, 'utils::file.edit', function(file, title=title) {
    task <- "C = A - B"
    deps <- c("A", "B")
    name <- "C"
    write(clutter_with_params(task, name, deps), file = file)
  })
  g <- .edita(g, "C")
  expect_equal(g@functions[["C"]], "C = A - B")
})


test_that("nothing changes if I don't modify a formula", { 
  skip_if_not(require(mockery), "mockery required")
  on.exit(delete_graph("test"))
  g <- setup("test")

  mock_edita <- mock(function(file, title = title) {
    task <- "C = (A + 1) * (B + 2)"
    name <- "C"
    deps <- c("A", "B")
    write(clutter_with_params(task, name, deps), file = file)
  })

  stub(.edita, "utils::file.edit", mock_edita)
  g <- .edita(g, "C")

  expect_true(!"C" %in% hash::keys(g@functions))
  expect_called(mock_edita, 1)
})
