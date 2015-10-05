\docType{methods}
\name{isElementary}
\alias{isElementary}
\title{Funzioni del package `grafo`}
\usage{
isElementary(graph, tsName)
}
\arguments{
  \item{graph}{un oggetto di classe('grafo')}

  \item{tsName}{nome della serie di cui si vuole conoscere
  se e' Elementare}
}
\description{
implementazione di isElementary di `package::grafo`
}
\note{
`GrafoDB` non prevede l'utilizzo di serie "elementari" come
il `grafo` Quindi per compliance ritorna sempre `FALSE`, ma
il metodo non ha senso
}
\seealso{
`grafo::isElementary`
}

