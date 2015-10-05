\name{is.grafodb}
\alias{is.grafodb}
\title{Controlla se `x` e' un `GrafoDB`}
\usage{
is.grafodb(x)
}
\arguments{
  \item{x}{un qualsiasi oggetto `R`}
}
\value{
`TRUE` se `x` e' un `GrafoDB`, altrimenti `FALSE`
}
\description{
Predicato; Ritorna `TRUE` se `x` e' un istanza di
`GrafoDB`, altrimenti ritorna `FALSE`
}
\examples{
\dontrun{
  g = GrafoDB()
  is.grafodb(g) # questo e' TRUE
  x = list()
  is.grafodb(x) # questo e' FALSE
}
}

