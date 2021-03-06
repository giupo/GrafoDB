% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core.r
\name{lookup}
\alias{lookup}
\alias{lookup,GrafoDB,character,character-method}
\alias{lookup,GrafoDB,numeric,missing-method}
\alias{lookup,GrafoDB,character,missing-method}
\title{ricerca nei metadati del `GrafoDB`}
\usage{
lookup(x, key, value)

\S4method{lookup}{GrafoDB,character,character}(x, key, value)

\S4method{lookup}{GrafoDB,numeric,missing}(x, key, value)

\S4method{lookup}{GrafoDB,character,missing}(x, key, value)
}
\arguments{
\item{x}{istanza di `GrafoDB`}

\item{key}{`character` che specifica la chiave del metadato}

\item{value}{`character` che specifica il valore del metadato}
}
\value{
un character array di nomi di serie che rispettano la
        clausola `key` = `value`.
        Se non esistono ritorna un character(0) (array vuoto)
}
\description{
ricerca nei metadati del `GrafoDB`
}
\examples{
\dontrun{
g = GrafoDB(...) # istanzia il grafo
lookup(g, "TAVOLA_DI_OUTPUT", "BRI") # ritorna i nomi di serie
                                     # che hanno TAVOLA_DI_OUTPUT=BRI
}
}
