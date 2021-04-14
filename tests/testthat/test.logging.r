context("Logging")

test_that("trace calls a logging function", {
  skip_if_not_installed("mockery")
  mock_logger <- mockery::mock(TRUE)
  mockery::stub(trace, "futile.logger::flog.trace", mock_logger)
  trace("test")
  expect_called(mock_logger, 1)
})

test_that("debug calls a logging function", {
  skip_if_not_installed("mockery")
  mock_logger <- mockery::mock(TRUE)
  mockery::stub(debug, "futile.logger::flog.debug", mock_logger)
  debug("test")
  expect_called(mock_logger, 1)
})

test_that("info calls a logging function", {
  skip_if_not_installed("mockery")
  mock_logger <- mockery::mock(TRUE)
  mockery::stub(info, "futile.logger::flog.info", mock_logger)
  info("test")
  expect_called(mock_logger, 1)
})

test_that("warn calls a logging function", {
  skip_if_not_installed("mockery")
  mock_logger <- mockery::mock(TRUE)
  mockery::stub(warn, "futile.logger::flog.warn", mock_logger)
  warn("test")
  expect_called(mock_logger, 1)
})

test_that("error calls a logging function", {
  skip_if_not_installed("mockery")
  mock_logger <- mockery::mock(TRUE)
  mockery::stub(error, "futile.logger::flog.error", mock_logger)
  error("test")
  expect_called(mock_logger, 1)
})

test_that("fatal calls a logging function", {
  skip_if_not_installed("mockery")
  mock_logger <- mockery::mock(TRUE)
  mockery::stub(fatal, "futile.logger::flog.fatal", mock_logger)
  fatal("test")
  expect_called(mock_logger, 1)
})