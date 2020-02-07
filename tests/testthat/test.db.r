context("DB related functions")

test_that("dbSettings can return custom home made ini settings", {
  skip_if_not(require(mockery), "mockery required")
  on.exit(dbSettings(flush=TRUE))
  expected <- list(A=1, B=1)
  options(dbSettings=NULL)
  stub(dbSettings, 'file.exists', function (...) TRUE)
  stub(dbSettings, 'ini_parse', function(...) expected)
  expect_equal(dbSettings(TRUE), expected)
})

test_that("initdb can handle an error", {
  dbSettings(TRUE)
  skip_if_not(require(mockery), "mockery requred")
  stub(initdb, 'getenv', function(...) stop("my error"))
  expect_error(initdb(buildConnection()), "my error")
})

test_that("shouldCreateSchema returns FALSE if dbReadTable succeed", {
  skip_if_not(require(mockery), "mockery required")
  mock <- mock(data.frame())
  stub(shouldCreateSchema, 'DBI::dbReadTable', mock)
  expect_error(shouldCreateSchema(NULL), NA)
  expect_called(mock, 1)
})

test_that("shouldCreateSchema returns TRUE if dbReadTable fails", {
  skip_if_not(require(mockery), "mockery required")
  mock  <- mock(stop('crap'))
  stub(shouldCreateSchema, 'DBI::dbReadTable', mock)
  expect_true(expect_error(shouldCreateSchema(NULL), NA))
  expect_called(mock, 1)
})

test_that("shouldCreateSchema returns False if dbReadTable raise a warning", {
  skip_if_not(require(mockery), "mockery required")
  mock <- mock(warning(ciao))
  stub(shouldCreateSchema, 'DBI::dbReadTable', mock)
  expect_true(shouldCreateSchema(NULL))
  expect_called(mock, 1)
})


test_that("initdbPostgreSQL calls system with the correct dbname (collaudo)", {
  skip_if_not(require(mockery), "mockery required")
  mock <- mock("")
  stub(initdbPostgreSQL, 'system', function(x) {
    expect_true(grepl("grafo_test",x))
  })
  initdbPostgreSQL(env="collaudo")
})

test_that("initdbPostgreSQL calls system with the correct dbname (prod)", {
  skip_if_not(require(mockery), "mockery required")
  mock <- mock("")
  stub(initdbPostgreSQL, 'system', function(x) {
    expect_true(!grepl("grafo_test",x))
  })
  initdbPostgreSQL(env="prod")
})

test_that("initdb is not called in buildConnection", {
  skip_if_not(require(mockery), "mockery required")
  mk <- mock(stop("crap"))
  stub(buildConnection, 'initdb', mk)
  con <- buildConnection()
  on.exit(disconnect(con))
  expect_called(mk, 0)
})

test_that("disconnect doesn't call disconnect if env is 'test'", {
  mk <- mock(function (...) {})
  stub(disconnect, 'dbDisconnect', mk)
  disconnect(NULL, env="test")
  expect_called(mk, 0)
})


test_that("disconnect calls disconnect if env is not 'test'", {
  mk <- mock(function (...) {})
  stub(disconnect, 'dbDisconnect', mk)
  disconnect(NULL, env="cippalippa")
  expect_called(mk, 1)
})
