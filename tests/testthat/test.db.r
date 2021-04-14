context("DB interface")

test_that("db_settings can return custom home made ini settings", {
  skip_if_not(require(mockery), "mockery required")
  on.exit(db_settings(flush = TRUE))
  expected <- list(A = 1, B = 1)
  options(db_settings = NULL)
  stub(db_settings, "file.exists", function(...) TRUE)
  stub(db_settings, "rutils::ini_parse", function(...) expected)
  expect_equal(db_settings(TRUE), expected)
})

test_that("initdb can handle an error", {
  db_settings(TRUE)
  skip_if_not(require(mockery), "mockery requred")
  stub(initdb, "getenv", function(...) stop("my error"))
  expect_error(initdb(build_connection()), "my error")
})

test_that("should_create_schema returns FALSE if dbReadTable succeed", {
  skip_if_not(require(mockery), "mockery required")
  mock <- mock(data.frame())
  stub(should_create_schema, "DBI::dbReadTable", mock)
  expect_error(should_create_schema(NULL), NA)
  expect_called(mock, 1)
})

test_that("should_create_schema returns TRUE if dbReadTable fails", {
  skip_if_not(require(mockery), "mockery required")
  mock  <- mock(stop("crap"))
  stub(should_create_schema, "DBI::dbReadTable", mock)
  expect_true(expect_error(should_create_schema(NULL), NA))
  expect_called(mock, 1)
})

test_that("should_create_schema returns False if dbReadTable raise a warning", {
  skip_if_not(require(mockery), "mockery required")
  mock <- mock(warning(ciao))
  stub(should_create_schema, "DBI::dbReadTable", mock)
  expect_true(should_create_schema(NULL))
  expect_called(mock, 1)
})


test_that("initdb_postgres calls system with the correct dbname (collaudo)", {
  skip_if_not(require(mockery), "mockery required")
  mock <- mock("")
  stub(initdb_postgres, "system", function(x) {
    expect_true(grepl("grafo_test", x))
  })
  initdb_postgres(env = "collaudo")
})

test_that("initdb_postgres calls system with the correct dbname (prod)", {
  skip_if_not(require(mockery), "mockery required")
  mock <- mock("")
  stub(initdb_postgres, "system", function(x) {
    expect_true(!grepl("grafo_test", x))
  })
  initdb_postgres(env = "prod")
})

test_that("initdb is not called in build_connection", {
  skip_if_not(require(mockery), "mockery required")
  mk <- mock(stop("crap"))
  stub(build_connection, "initdb", mk)
  con <- build_connection()
  on.exit(disconnect(con))
  expect_called(mk, 0)
})

test_that("disconnect doesn't call disconnect if env is 'test'", {
  mk <- mock(function(...) {
  })
  stub(disconnect, "dbDisconnect", mk)
  disconnect(NULL, env = "test")
  expect_called(mk, 0)
})


test_that("disconnect calls disconnect if env is not 'test'", {
  mk <- mock(function(...) {
  })
  stub(disconnect, "DBI::dbDisconnect", mk)
  disconnect(NULL, env = "cippalippa")
  expect_called(mk, 1)
})
