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
  skip_if_not(require(mockery), "mockery required")
  stub(initdb, 'stringr::str_trim', function(...) stop("my error"))
  expect_error(initdb(pgConnect()), "my error")
})
