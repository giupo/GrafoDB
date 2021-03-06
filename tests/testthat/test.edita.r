setup <- function(tag) {
  g <- GrafoDB(tag)
  g["A"] <- g["B"] <- stats::ts(c(0, 0, 0), start = c(1990, 1), frequency = 1)
  g["C"] <- function(A, B) { # nolint
    C = (A + 1) * (B + 2) # nolint
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

  mockery::stub(.edita, "utils::file.edit", function(file, title = title) {
    message(file)
    task <- "C = A / B"
    deps <- c("A", "B")
    write(clutter_with_params(task, deps), file = file)
  })

  g <- .edita(g, "C")
  expect_equal(g@functions[["C"]], "C = A/B")
})

test_that("I can edit a function 2", {
  skip_if_not(require(mockery), "mockery required")
  on.exit(delete_graph("test"))
  g <- setup("test")

  mockery::stub(.edita, "utils::file.edit", function(file, title=title) {
    task <- "D = A - B"
    deps <- c("A", "B")
    write(clutter_with_params(task, deps), file = file)
  })
  g <- .edita(g, "D")
  expect_equal(g@functions[["D"]], "D = A - B")
})

test_that("I can replace a function", {
  skip_if_not(require(mockery), "mockery required")
  on.exit(delete_graph("test"))
  g <- setup("test")

  mockery::stub(.edita, "utils::file.edit", function(file, title=title) {
    task <- "C = A - B"
    deps <- c("A", "B")
    write(clutter_with_params(task, deps), file = file)
  })
  g <- .edita(g, "C")
  expect_equal(g@functions[["C"]], "C = A - B")
})


test_that("nothing changes if I don't modify a formula", {
  skip_if_not(require(mockery), "mockery required")
  on.exit(delete_graph("test"))
  g <- setup("test")

  mock_edita <- mockery::mock(function(file, title = title) {
    task <- "C = (A + 1) * (B + 2)"
    deps <- c("A", "B")
    write(clutter_with_params(task, deps), file = file)
  })

  mockery::stub(.edita, "utils::file.edit", mock_edita)
  g <- .edita(g, "C")

  expect_true(!"C" %in% hash::keys(g@functions))
  mockery::expect_called(mock_edita, 1)
})
