test_that("I can init an sql helper", {
  sql_helper <- SQLHelper()
  expect_true(class(sql_helper) == "SQLHelper")
})

test_that("SQLHelper raises an error if sql key is not present", {
  helper <- SQLHelper()
  expect_error(sql_by_key(helper, "NONESISTO"))
})

test_that("SQLHelper gets different queries based on type", {
  sqlite <- SQLHelper(type = "SQLite")
  psql <- SQLHelper(type = "PostgreSQL")
  expect_true(sqlite@type != psql@type)

  sql0 <- sql_by_key(sqlite, "SELECT_TEST0")
  sql1 <- sql_by_key(psql, "SELECT_TEST0")
  expect_true(sql0 != sql1)

  sql0 <- sql_by_key(sqlite, "SELECT_TEST1")
  sql1 <- sql_by_key(psql, "SELECT_TEST1")
  expect_equal(sql0, sql1)
})

test_that("I can have params to be substitued into sql", {
  helper <- SQLHelper(type = "SQLite")
  expect_equal(helper@type, "SQLite")

  expect_equal(
    sql_by_key(helper, "SELECT_TEST2", tabella = "A"),
    "select * from A")

  expect_equal(
    sql_by_key(helper, "SELECT_TEST2", tabella = "B"),
    "select * from B")
})

test_that("SQLHelper can handle quoted values", {
  helper <- SQLHelper(type = "SQLite")
  sql <- sql_by_key(helper, "SELECT_TEST3", tabella = "A", name = "d'aglio")
  expect_equal(sql, "select * from A where name='d''aglio'")
})

test_that("SQLHelper can handle multiple quoted values", {
  helper <- SQLHelper(type = "SQLite")
  sql <- sql_by_key(helper,
    "SELECT_TEST3",
    tabella = "A",
    name = "d'odio e d'amore")

  expect_equal(sql, "select * from A where name='d''odio e d''amore'")
})

test_that("SQLHelper raises an error if params are not set", {
  helper <- SQLHelper(type = "SQLite")
  expect_warning(expect_error(
    sql_by_key(helper, "SELECT_TEST3"),
    "params for query SELECT_TEST3 of type SQLite have not been set"))
})

test_that("sql_helper_type_by_env returns SQLite for test", {
  skip_if_not_installed("mockery")
  getenv_mock <- mockery::mock("test")
  expect_equal(sql_helper_type_by_env("test"), "SQLite")
  mockery::stub(sql_helper_type_by_env, "getenv", getenv_mock)
  expect_equal(sql_helper_type_by_env(), "SQLite")
  mockery::expect_called(getenv_mock, 1)
})

test_that("sql_helper_type_by_env returns SQLite for prod", {
  skip_if_not_installed("mockery")
  getenv_mock <- mockery::mock("prod")
  expect_equal(sql_helper_type_by_env("prod"), "PostgreSQL")
  mockery::stub(sql_helper_type_by_env, "getenv", getenv_mock)
  expect_equal(sql_helper_type_by_env(), "PostgreSQL")
  mockery::expect_called(getenv_mock, 1)
})

test_that("assert_sql_params fails with params not set", {
  sql <- "select * from universe where black_hole = '--MURPH--'"
  expect_warning(expect_error(assert_sql_params(sql)))
})

test_that("assert_sql_params does nothing with params set", {
  sql <- "select * from universe where black_hole = 'HELLO'"
  expect_warning(expect_error(assert_sql_params(sql), NA), NA)
  expect_true(assert_sql_params(sql))
})