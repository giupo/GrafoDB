context("Funzioni per la persistenza")

g <- GrafoDB("test")
g["A"] <- ts(runif(10), start=c(1990,1), freq=4)
g["B"] <- ts(runif(10), start=c(1990,1), freq=4)
g["C"] <- function(A,B) {
  C = A + B    
}

g["D"] <- ts(c(NA,1,NA), start=c(1990,1), freq=4)

g <- saveGraph(g)

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
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  dbGetQuery(con, "update dati set dati=replace(dati, 'null', 'NaN') where tag='test'")
  
  saveGraph(g)
  g1 <- GrafoDB("test")
  expect_true( any( is.na(g[["D"]] )))
  expect_true( length(g[["D"]])>0 )
})

for(tag in rilasci("test")$tag) elimina(tag)

setup <- function(tag) {
  g <- GrafoDB(tag)
  g["A"] <- ts(runif(10), start=c(1990,1), freq=4)
  g["B"] <- ts(1:10, start=c(1990,1), freq=4)
  g["C"] <- function(A,B) {
    C = A + B
  }
  
  g["D"] <- function(B) {
    D =  B * 2
  }

  g
}

test_that("I can save a graph over another existing graph", {
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

for(tag in rilasci("test")$tag) elimina(tag)

