#!/usr/bin/env Rscript

sessionInfo()

suppressMessages(library(testthat))
suppressMessages(library(rdataset))
suppressMessages(library(stringr))
suppressMessages(library(methods))
suppressMessages(library(rutils))
suppressMessages(library(hash))
suppressMessages(library(RSQLite))
suppressMessages(library(igraph))
suppressMessages(library(futile.logger))

devtools::load_all()

flog.info("GRAFODB_ENV:%s", Sys.getenv("GRAFODB_ENV"))
invisible(flog.threshold(WARN, name='GrafoDB.sqlhelper'))
invisible(flog.threshold(WARN, name='GrafoDB.db'))
invisible(flog.threshold(WARN, name='GrafoDB.persistence.saveGraph'))
invisible(flog.threshold(TRACE, name='GrafoDB.patch'))

auto_test("R", "tests/testthat/")
