context("SQLHelper functions")

test_that("I can init an sql helper", {
  sqlHelper <- SQLHelper()
  expect_true(class(sqlHelper) == "SQLHelper")
})

test_that("SQLHelper defaults to PostgreSQL without env", {

  called <<- FALSE
  with_mock('getenv' = function(...) {
    called <<- TRUE
    "prod"
  }, {
    sqlHelper <- SQLHelper()
    expect_true(called)
    expect_equal(sqlHelper@type, "PostgreSQL")
  })
})


test_that("SQLHelper sets based on env", {
  called <<- FALSE
  with_mock(
    'getenv' = function(...) {
      called <<- TRUE
      "test"
    }, {
      expect_error(SQLHelper(), NA)
      expect_equal(SQLHelper()@type, "SQLite")
      expect_true(called)
    })
})

test_that("SQLHelper raises an error if sql key is not present", {
  helper <- SQLHelper()
  expect_error(getSQLbyKey(helper, "NONESISTO"))
})

test_that("SQLHelper gets different queries based on type", {
  oldOption <- getOption("SQLHelperType", NULL)
  sqlite <- SQLHelper(type="SQLite")
  psql <- SQLHelper(type="PostgreSQL")
  expect_true(sqlite@type != psql@type)
  
  sql0 <- getSQLbyKey(sqlite, "SELECT_TEST0")
  sql1 <- getSQLbyKey(psql, "SELECT_TEST0")
  expect_true(sql0 != sql1)
  
  sql0 <- getSQLbyKey(sqlite, "SELECT_TEST1")
  sql1 <- getSQLbyKey(psql, "SELECT_TEST1")
  expect_equal(sql0, sql1)
})

test_that("I can have params to be substitued into sql", {
  helper <- SQLHelper(type="SQLite")
  expect_equal(helper@type, "SQLite")

  expect_equal(
    getSQLbyKey(helper, "SELECT_TEST2", tabella="A"),
    "select * from A")

  expect_equal(
    getSQLbyKey(helper, "SELECT_TEST2", tabella="B"),
    "select * from B")
})

test_that("SQLHelper can handle quoted values", {
  helper <- SQLHelper(type="SQLite")
  sql <- getSQLbyKey(helper, "SELECT_TEST3", tabella = "A", name= "d'aglio")
  expect_equal(sql, "select * from A where name='d''aglio'")
})

test_that("SQLHelper can handle multiple quoted values", {
  helper <- SQLHelper(type="SQLite")
  sql <- getSQLbyKey(helper, "SELECT_TEST3", tabella = "A", name = "d'odio e d'amore")
  expect_equal(sql, "select * from A where name='d''odio e d''amore'")
})

test_that("SQLHelper raises an error if params are not set", {
  helper <- SQLHelper(type="SQLite")
  expect_warning(expect_error(
    getSQLbyKey(helper, "SELECT_TEST3"),
    "params for query SELECT_TEST3 of type SQLite have not been set"))
})
