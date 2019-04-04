context('Diff functions')

setup <- function(tag) {
  g <- GrafoDB(tag)
  g["A"] <- ts(runif(10), start=c(1990,1), frequency=4)
  g["B"] <- ts(runif(10), start=c(1990,1), frequency=4)
  g["C"] <- function(A, B) {
    C = A + B
  }
  g
}

test_that("I get no diff on the same Graph", {
  g <- setup("test")
  on.exit(elimina("test"))
  
  expect_equal(nrow(diff.GrafoDB(g, g)), 0)
})

test_that("I get a warning for uncommon names between graphs", {
  g <- setup("test")
  g1 <- setup("test2")

  on.exit({
    elimina("test")
    elimina("test2")
  })
  
  g1["D"] <- function(A) {
    D = A * 2
  }
  expect_warning(diff.GrafoDB(g, g1))
  expect_equal(nrow(diff.GrafoDB(g, g1)), 0)
})

test_that("I get a dataframe with the difference in formulas", {
  g <- setup("test")
  g1 <- setup("test2")

  on.exit({
    elimina("test")
    elimina("test2")
  })
  saveGraph(g)
  saveGraph(g1)

  g1["C"] <- function(A, B) {
    C = A - B
  }

  saveGraph(g1)
  expect_equal(nrow(diff.GrafoDB(g, g1)), 1)
})

test_that("I get authors in each data.frame", {
  g <- setup("test")
  g1 <- setup("test2")

  on.exit({
    elimina("test")
    elimina("test2")
  })

  saveGraph(g)
  saveGraph(g1)

  g1["C"] <- function(A, B) {
    C = A - B
  }
  saveGraph(g1)
  
  dd <- diff.GrafoDB(g, g1)
  expect_true("test_autore" %in% colnames(dd))
  expect_true("test2_autore" %in% colnames(dd))
})
