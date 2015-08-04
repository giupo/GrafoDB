context("Funzioni per la persistenza")

g <- GrafoDB("test")
g["A"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
g["B"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
g["C"] <- function(A,B) {
  C = A + B    
}

g["D"] <- TSERIES(c(NA,1,NA), START=c(1990,1), FREQ=4)

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

# elimina("test")
