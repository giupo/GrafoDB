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

test_that("init_db can handle an error", {
  dbSettings(TRUE)
  skip_if_not(require(mockery), "mockery required")
  stub(initdb, 'str_trim', function(...) stop("my error"))
  expect_error(initdb(pgConnect()), "my error")
})

test_that("shouldCreateSchema returns FALSE if dbGetQuery succeed", {
  skip_if_not(require(mockery), "mockery required")
  mock <- mock(data.frame())
  stub(shouldCreateSchema, 'dbGetQuery', mock)

  expect_error(shouldCreateSchema(NULL), NA)
  expect_called(mock, 1)
})

test_that("shouldCreateSchema returns TRUE if dbGetQuery fails", {
  skip_if_not(require(mockery), "mockery required")
  mock  <- mock(stop('crap'))
  stub(shouldCreateSchema, 'dbGetQuery', mock)
  expect_true(expect_error(shouldCreateSchema(NULL), NA))
  expect_called(mock, 1)
})

test_that("shouldCreateSchema returns False if dbGetQuery raise a warning", {
  skip_if_not(require(mockery), "mockery required")
  mock <- mock(warning(ciao))
  stub(shouldCreateSchema, 'dbGetQuery', mock)
  expect_true(shouldCreateSchema(NULL))
  expect_called(mock, 1)
})
