% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/persistence.r
\name{do_history}
\alias{do_history}
\title{Esegue il rolling dei vintage del `GrafoDB`}
\usage{
do_history(x, tag, con)
}
\arguments{
\item{x}{istanza di `GrafoDB`}

\item{tag}{tag del grafo}

\item{con}{connessione al database}
}
\description{
Ad ogni salvataggio con il metodo `saveGraph` se non impostiamo
un nuovo `tag` il `GrafoDB` salva i dati sullo stesso `tag` ma
contemporaneamente salva la versione precedente con un progressivo,
in modo da tener traccia di chi ha fatto cosa nel tempo.
}
\details{
Le versioni sono contraddistinte da un nuovo tag, `tag`p`X` dove
`X` e' un numero progressivo

Il grafo potra' successivamente essere caricato con il nuovo tag.
}
\note{
questa e' una funzione interna del grafo invocata da `update_graph`
}
\seealso{
saveGraph update_graph
}
