#!/usr/bin/env Rscript

#sessionInfo()

suppressMessages(library(testthat))
#suppressMessages(library(rdataset))
#suppressMessages(library(stringr))
#suppressMessages(library(methods))
#suppressMessages(library(rutils))
#suppressMessages(library(hash))
#suppressMessages(library(RSQLite))
#suppressMessages(library(igraph))

# devtools::load_all()

Sys.setenv(GRAFODB_ENV="test")
futile.logger::flog.info("GRAFODB_ENV: %s", Sys.getenv("GRAFODB_ENV"))

invisible(futile.logger::flog.threshold(futile.logger::WARN, name='GrafoDB.sqlhelper'))
invisible(futile.logger::flog.threshold(futile.logger::WARN, name='GrafoDB.db'))
invisible(futile.logger::flog.threshold(futile.logger::WARN, name='GrafoDB.persistencesave_graph_impl'))
invisible(futile.logger::flog.threshold(futile.logger::WARN, name='GrafoDB.patch'))

## options(warn=2)
## auto_test("R/", "tests/testthat/")
auto_test_package()
