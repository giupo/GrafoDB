% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.r
\name{to_function_as_string}
\alias{to_function_as_string}
\title{Orla le formule del grafo in funzioni}
\usage{
to_function_as_string(func_string, name, func_name = "proxy")
}
\arguments{
\item{func_string}{character array che rappresenta la funzione}

\item{name}{name of the object to be returned}

\item{func_name}{name of the function (`proxy` default)}
}
\value{
un character array della funzione orlata
}
\description{
Questa funzione orla le funzioni del grafo con
`proxy <-function() {` e `}` finale.
}
\details{
Le istruzioni vengono incapsulate in una funzione generica chiamata proxy.
gli argomenti devono essere definiti prima nella ambiente per la
corretta esecuzione
}
\note{
funzione interna
}
