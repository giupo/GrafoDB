context("DB related functions")

test_that("dbSettings can return custom home made ini settings", {
  on.exit(dbSettings(flush=TRUE))
  expected <- list(A=1, B=1)
  options(dbSettings=NULL)
  with_mock(
    'base::file.exists' = function(...) TRUE,
    'rutils::ini_parse' = function(...) expected, {
      expect_equal(dbSettings(TRUE), expected)
    })
})

test_that("init_db can handle an error", {
  with_mock(
    'stringr::str_trim' = function(...) stop("my error"),  {
      expect_error(initdb(pgConnect()), "my error")
  })
})
