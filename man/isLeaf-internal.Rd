% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.r
\name{.isLeaf}
\alias{.isLeaf}
\title{Controlla se un nodo e' una foglia}
\usage{
.isLeaf(x, i)
}
\arguments{
\item{x}{istanza di `GrafoDB`}

\item{i}{array di `character` con i nomi delle serie su cui si vuole
applicare il predicato}
}
\value{
vector di `logical` (stessa lunghezza di `i`) con i risultati
        del controllo
}
\description{
Ritorna un array di `logical` uno per ogni elemento in `i`: `TRUE`
se l'i-esimo elemento e' una foglia (non ha archi uscenti),
altrimenti `FALSE`
}
