\docType{methods}
\name{evaluate}
\alias{evaluate}
\title{Implementazione del generic `evaluate` del package `grafo`
per la classe `GrafoDB`}
\usage{
evaluate(object)
}
\arguments{
  \item{object}{L'istanza di grafo che si vuole valutare}

  \item{v_start}{Il vertice di partenza per la valutazione
  o una lista di vertici}
}
\description{
Esegue il calcolo delle serie storiche presenti in questo
Database
}
\note{
il generic e' definito nel package `grafo`
}
\examples{
\dontrun{
g <- GrafoDB(...) # istanzia il grafo
g <- evaluate(g)  # lo valuta tutto
v_start <- "ZERIQ"
g <- evaluate(g, v_start) # lo valuta solo a partire da ZERIQ
}
}
\seealso{
.evaluate
}

