library(testthat)
library(GrafoDB)
Sys.setenv(GRAFODB_ENV="test")
test_check("GrafoDB")
