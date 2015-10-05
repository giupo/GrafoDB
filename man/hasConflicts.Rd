\docType{methods}
\name{hasConflicts}
\alias{hasConflicts}
\title{Ritorna `TRUE` se il grafo ha conflitti}
\usage{
hasConflicts(x)

hasConflicts(x)
}
\arguments{
  \item{x}{oggetto R}

  \item{name}{character array di nomi (puo' essere omesso)}

  \item{x}{oggetto R}

  \item{name}{character array di nomi (puo' essere omesso)}
}
\value{
`TRUE` se l'istanza `x` e' un GrafoDB con conflitti,
`FALSE` altrimenti

`TRUE` se l'istanza `x` e' un GrafoDB con conflitti,
`FALSE` altrimenti
}
\description{
Ritorna `TRUE` se il grafo ha conflitti

Ritorna `TRUE` se il grafo ha conflitti
}
\examples{
\dontrun{
g <- GrafoDB(...)
hasConflicts(g) # dovrebbe essere FALSE
...             # eseguo operazioni come modificare la stessa serie
hasConflicts(g) # TRUE
hasConflicts(g, SERIE_CON_CONFLITTO) # TRUE
hasConflicts(g, SERIE_SENZA_CONFLITTO) # FALSE
}
\dontrun{
g <- GrafoDB(...)
hasConflicts(g) # dovrebbe essere FALSE
...             # eseguo operazioni come modificare la stessa serie
hasConflicts(g) # TRUE
hasConflicts(g, SERIE_CON_CONFLITTO) # TRUE
hasConflicts(g, SERIE_SENZA_CONFLITTO) # FALSE
}
}

