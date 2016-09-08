#!/usr/bin/env Rscript

library(testthat)
library(devtools)
library(methods)
library(DBI)
library(RSQLite)
library(RPostgreSQL)
library(doMC)
library(zoo)
library(tis)

source("lib_mgt.r")

load_all()

auto_test("R", "tests/testthat/")
