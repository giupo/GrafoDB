context("Funzioni per la persistenza")

setup <- function(name) {
  g <- GrafoDB(name)
  g["A"] <- ts(runif(10), start=c(1990,1), frequency=4)
  g["B"] <- ts(runif(10), start=c(1990,1), frequency=4)
  g["C"] <- function(A,B) {
    C = A + B    
  }
  
  g["D"] <- ts(c(NA,1,NA), start=c(1990,1), frequency=4)
  
  g <- saveGraph(g)
  g
}

g <- setup("test")

test_that("Posso salvare e ricaricare da un file", {
  path <- tempfile()
  saveBinary(g, path)
  x <- readBinary(path)
  expect_true(is.grafodb(x))
  unlink(path)
})

test_that("I can handle NaN despite JsonCpp, RJSONIO, IEEE754", {
  ## il problema qui e' che quando serializzo python giustamente
  ## usa 'NaN' per i missing. mentre C++/R preferiscono 'null'
  on.exit({
    for(tag in rilasci("test")$tag) elimina(tag)
  })
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  dbExecute(
	con,
	"update dati set dati=replace(dati, 'null', 'NaN') where tag='test'")

  saveGraph(g)
  g1 <- GrafoDB("test")
  expect_true( any( is.na(g[["D"]] )))
  expect_true( length(g[["D"]])>0 )
})

setup <- function(tag) {
  g <- GrafoDB(tag)
  g["A"] <- ts(runif(10), start=c(1990,1), frequency=4)
  g["B"] <- ts(1:10, start=c(1990,1), frequency=4)
  g["C"] <- function(A,B) {
    C = A + B
  }
  
  g["D"] <- function(B) {
    D =  B * 2
  }

  g
}

test_that("I can save a graph over another existing graph", {
  on.exit({
    for(tag in rilasci("test")$tag) elimina(tag)
  })
  tagA <- "test2"
  tagB <- "test3"
  ga <- setup(tagA)
  gb <- setup(tagB)
 
  saveGraph(ga)
  saveGraph(gb)

  saveGraph(ga, tagB)
  
  g <- GrafoDB(tagB)
  expect_true(all(g$B == gb$B))
})

elimina("test")

context("Funzioni per la persistenza [internal functions]")

test_that("loadTable stops if tableName doesn't exists", {
  expect_error(loadTable("nonesisto", "nonesisto"))
})

test_that("Load* yields an empy dataframe in case of error", {
  ## I don't like this at all.

  with_mock(
    'GrafoDB:::loadTable' = function(...) stop("my error") , {
      x <- loadDati("nonsense")
      expect_is(x, "data.frame")
      expect_equal(nrow(x), 0)

      x <- loadArchi("nonesisto")
      expect_is(x, "data.frame")
      expect_equal(nrow(x), 0)

      x <- loadFormule("nonesisto")
      expect_is(x, "data.frame")
      expect_equal(nrow(x), 0)
    })
})

test_that("need_resync returns true if GrafoNeeds a resync", {
  g <- setup("test")
  on.exit({
    for(tag in rilasci("test")$tag) elimina(tag)
  })
  
  saveGraph(g)

  g1 <- GrafoDB("test")
  g2 <- GrafoDB("test")
  g2["A"] <- ts(runif(10), start=c(1990,1), freq=4)
  saveGraph(g2)
  expect_true(need_resync(g1))
})
