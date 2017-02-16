context("edit functions")

setup <- function(tag) {
  g <- GrafoDB(tag)
  g["A"] <- g["B"] <- ts(c(0,0,0), start=c(1990,1), freq=1)
  g["C"] <- function(A, B) {
    C = (A + 1) * (B + 2)
  }
  saveGraph(g)
  
  setMeta(g, "A", "KEY", "VALUE1")
  setMeta(g, "A", "KEY", "VALUE2")
  setMeta(g, "B", "KEY", "VALUE1")
  g
}


test_that("I can edit a function", {
  on.exit(elimina("test"))
  g <- setup("test")
  with_mock(
    'utils::file.edit' = function(file, title) {
      write("function(A, B)\n{ C = A/B }", file=file)
    }, {
      edita(g, "C")
      expect_equal(g@functions[["C"]], "C = A/B")
    })

  with_mock(
    'utils::file.edit' = function(file, title) {
      write("function(A, B)\n{ D = A - B }", file=file)
    }, {
      edita(g, "D")
      expect_equal(g@functions[["D"]], "D = A - B")
      expect_true("D" %in% V(g@network)$name)
    })
  with_mock(
    'utils::file.edit' = function(file, title) {
      write("function(A, B)\n{ C = A /// B }", file=file)
    }, {
      expect_error(edita(g, "C"))
      expect_equal(g@functions[["C"]], "C = A /// B")
    })

  expect_error(edita(g, "A"), "primitiva")
})

test_that("nothing changes if I don't modify a formula", {
  on.exit(elimina("test"))
  g <- setup("test")
  with_mock(
    'utils::file.edit' = function(file, title) {
      write("function(A, B)\n{ C = (A + 1) * (B + 2) }", file=file)
    }, {
      edita(g, "C")
      expect_true(!"C" %in% keys(g@functions))
    })
})
