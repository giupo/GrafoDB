\name{.clutter_function}
\alias{.clutter_function}
\title{Questa funzione orla le funzioni del grafo con `proxy <-function() {` e `}` finale.}
\usage{
.clutter_function(f)
}
\arguments{
  \item{f}{character array che rappresenta la funzione}

  \item{name}{name of the object to be returned}

  \item{funcName}{name of the function (`proxy` default)}
}
\value{
un character array della funzione orlata
}
\description{
Le istruzioni vengono incapsulate in una funzione generica
chiamata proxy. gli argomenti devono essere definiti prima
nella ambiente per la corretta esecuzione
}
\note{
funzione interna
}

