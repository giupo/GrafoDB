context("edit functions")

setup <- function(tag) {
  g <- GrafoDB(tag)
  g["A"] <- g["B"] <- ts(c(0,0,0), start=c(1990,1), frequency=1)
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
  skip("mockery can't stub a S4 generic implementation")
  skip_if_not(require(mockery), "mockery required")
  on.exit(elimina("test"))
  g <- setup("test")

  stub(edita, 'utils::file.edit', function(file, title) {
    write("function(A, B)\n{ C = A/B }", file=file)
  })
  edita(g, "C")
  expect_equal(g@functions[["C"]], "C = A/B")
  stub(edita, 'file.edit', function(file, title) {
    write("function(A, B)\n{ D = A - B }", file=file)
  })
  edita(g, "D")
  expect_equal(g@functions[["D"]], "D = A - B")
  expect_true("D" %in% V(g@network)$name)

  stub(edita, 'utils::file.edit', function(file, title) {
    write("function(A, B)\n{ C = A /// B }", file=file)
  })
  expect_error(edita(g, "C"))
  expect_equal(g@functions[["C"]], "C = A /// B")
})

test_that("I can edit a root, making it a formula", {
  skip("mockery can't stub a S4 generic implementation")    
  skip_if_not(require(mockery), "mockery required")
  on.exit(elimina("test"))
  g <- setup("test")
  stub(edita, 'utils::file.edit', function(file, title) {
    write("function()\n{ A = 1 }", file=file)
  })
  expect_equal(deps(g, "A"), c())
  edita(g, "A")
  expect_equal(g@functions[["A"]], "A = 1")
})

test_that("nothing changes if I don't modify a formula", {
  skip("mockery can't stub a S4 generic implementation")    
  skip_if_not(require(mockery), "mockery required")
  on.exit(elimina("test"))
  g <- setup("test")
  stub(edita, 'utils::file.edit', function(file, title) {
    write("function(A, B)\n{ C = (A + 1) * (B + 2) }", file=file)
  })
  edita(g, "C")
  expect_true(!"C" %in% keys(g@functions))
})

test_that("If I edit a root, and I do nothing, still keep the node as root and nothing changes", {
  skip("mockery can't stub a S4 generic implementation")    
  skip_if_not(require(mockery), "mockery required")
  on.exit(elimina("test"))
  g <- setup("test")
  stub(edita, 'utils::file.edit', function(file, title) {
    write("function() { A = ... # work it\n}", file=file)
    })
  expect_error(edita(g, "A"))
  expect_true(!"A" %in% keys(g@functions))
})
