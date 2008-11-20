\name{cloneaquaenv}
\alias{cloneaquaenv}
\title{cloneaquaenv}
\description{PRIVATE function: clones an object of class aquaenv: it is possible to supply a new value for either TA or pH; the switches speciation, skeleton, revelle, and dsa are obtained from the object to be cloned}
\usage{cloneaquaenv(aquaenv, TA=NULL, pH=NULL, k_co2=NULL, k1k2="roy", khf="dickson")}
\arguments{
  \item{aquaenv }{object of class aquaenv}
  \item{TA }{optional new value for TA}
  \item{pH }{optional new value for pH}
  \item{k_co2}{used for TA fitting: give a K\_CO2 and NOT calculate it from T and S: i.e. K\_CO2 can be fitted in the routine as well}
  \item{k1k2}{either "roy" (default, Roy1993a) or "lueker" (Lueker2000, calculated with seacarb) for K\_CO2 and K\_HCO3.}
  \item{khf}{either "dickson" (default, Dickson1979a) or "perez" (Perez1987a, calculated with seacarb) for K\_HF}
}
\value{
  cloned object of class aquaenv
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\keyword{misc}

