
context("is_error")

test_that("test that is_error works", {
  tryCatch({
    stop("hello!")
  }, error = function(cond) {
    expect_true(is_error(cond))
  })

  expect_false(is_error())
  expect_false(is_error(NA))
  expect_false(is_error(1))
  expect_false(is_error(list()))
  expect_false(is_error(data.frame()))
  expect_false(is_error(FALSE))
  expect_false(is_error(""))
})

context("find_dep_from_error")

test_that("find_dep_from_error doesn't work with anything but errors", {
  tryCatch({
    stop("pippo pluto paperino: object 'X' not found")
  }, error = function(cond) {
    expect_equal(find_dep_from_error(cond), "X")
  })
  tryCatch({
    stop("pippo pluto paperino: object 'x' not found")
  }, error = function(cond) {
    expect_equal(find_dep_from_error(cond), "x")
    expect_false(find_dep_from_error(cond) ==  "X")
  })
  tryCatch({
    stop("pippo pluto paperino")
  }, error = function(cond) {
    expect_true(is.na(find_dep_from_error(cond)))
  })
  expect_error(find_dep_from_error(), "`cond` is not an error")
  expect_error(find_dep_from_error(NA), "`cond` is not an error")
  expect_error(find_dep_from_error(1), "`cond` is not an error")
  expect_error(find_dep_from_error(list()), "`cond` is not an error")
  expect_error(find_dep_from_error(data.frame()), "`cond` is not an error")
  expect_error(find_dep_from_error(FALSE), "`cond` is not an error")
  expect_error(find_dep_from_error(""), "`cond` is not an error")
})

context("find_deps")

test_that("find_deps throws an error if objects are not found", {
  data <- list(x = 1, y = 2)
  formula <- "dd = x + y + b"
  expect_error(find_deps(data, formula), "I dunno where to look for: b")
})

test_that("find_deps works as expected", {
  data <- list(x = 1, y = 2)
  formula <- "dd = x + y"
  expect_equal(find_deps(data, formula), c("x", "y"))
  expect_false("dd" %in% find_deps(data, formula))

  formula <- "dd = x"
  expect_equal(find_deps(data, formula), "x")
  expect_false("dd" %in% find_deps(data, formula))
})

test_that("if formula is NULL, deps are NULL", {
  data <- list(x = 1, y = 2)
  expect_null(find_deps(data, NULL))
})