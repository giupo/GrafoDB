% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db.r
\name{postgres_connect}
\alias{postgres_connect}
\title{Builds a connection to Postgres}
\usage{
postgres_connect()
}
\value{
a DBI connection to postgres
}
\description{
Builds a connection to Postgres
}
\note{
fails if RPostgreSQL is not installed
   the connection params are delegated to the libpq environment variables
   https://www.postgresql.org/docs/9.3/libpq-envars.html
}
