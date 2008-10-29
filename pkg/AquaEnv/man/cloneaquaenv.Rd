\name{cloneaquaenv}
\alias{cloneaquaenv}
\title{cloneaquaenv}
\description{PRIVATE function: clones an object of class aquaenv: it is possible to supply a new value for either TA or pH; the switches speciation, skeleton, revelle, and dsa are obtained from the object to be cloned}
\usage{cloneaquaenv(aquaenv, TA=NULL, pH=NULL)}
\arguments{
  \item{aquaenv }{object of class aquaenv}
  \item{TA }{optional new value for TA}
  \item{pH }{optional new value for pH}
}
\value{
  cloned object of class aquaenv
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\keyword{misc}

