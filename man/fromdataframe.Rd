% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.r
\name{from_data_frame}
\alias{from_data_frame}
\title{converte un dataframe (caricato dal Database) in una timeseries `ts`}
\usage{
from_data_frame(df)
}
\arguments{
\item{df}{data.frame compilato dal database}
}
\description{
converte un dataframe (caricato dal Database) in una timeseries `ts`
}
\note{
i dati dal db sono memorizzati come stringhe JSON
}
