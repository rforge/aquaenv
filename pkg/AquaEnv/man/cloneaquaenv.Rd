\name{cloneaquaenv}
\alias{cloneaquaenv}
\title{cloneaquaenv}
\description{PRIVATE function: clones an object of class aquaenv: it is possible to supply a new value for either TA or pH; the switches speciation, skeleton, revelle, and dsa are obtained from the object to be cloned}
\usage{cloneaquaenv(aquaenv, TA=NULL, pH=NULL, K1=NULL)}
\arguments{
  \item{aquaenv }{object of class aquaenv}
  \item{TA }{optional new value for TA}
  \item{pH }{optional new value for pH}
  \item{K1}{used for TA fitting: give a K\_CO2 and NOT calculate it from T and S: i.e. K\_CO2 can be fitted in the routine as well}
}
\value{
  cloned object of class aquaenv
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\keyword{misc}

