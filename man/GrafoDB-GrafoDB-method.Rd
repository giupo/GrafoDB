% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core.r
\name{-,GrafoDB,GrafoDB-method}
\alias{-,GrafoDB,GrafoDB-method}
\title{Esegue la differenza tra due grafi}
\usage{
\S4method{-}{GrafoDB,GrafoDB}(e1, e2)
}
\arguments{
\item{e1}{instance of GrafoDB}

\item{e2}{instance of GrafoDB}
}
\description{
Esegue la differenza tra due grafi
}
\examples{
\dontrun{
 nasecg <- GrafoDB("nasecg2004")
 cf10 <- GrafoDB()
 diff <- cf10 - nasecg
}
}
