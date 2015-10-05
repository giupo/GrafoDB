\docType{methods}
\name{setMeta}
\alias{setMeta}
\title{Imposta un metadato per una particolare serie}
\usage{
\S4method{setMeta}{GrafoDB,character,character,character}(object, tsName,
  attrName, value)
}
\arguments{
  \item{object}{un oggetto di classe('grafo')}

  \item{tsName}{nome di una serie del grafo}

  \item{attrName}{nome di un metadato della serie}

  \item{value}{nuovo valore o nuova lista di valori da
  attribuire al metadato}
}
\value{
il GrafoDB modificato
}
\description{
Imposta un metadato `attrName`=`value` per la serie
`tsName` Se la serie non esiste, va in errore.
}
\examples{
\dontrun{
g <- GrafoDB()
g["A"] <- TSERIES(...)
g <- setMeta(g, "A", "FONTE", "CODICE_FONTE")

searchNode(g, "FONTE", "CODICE_FONTE") # deve tornare il nome "A"
}
}

