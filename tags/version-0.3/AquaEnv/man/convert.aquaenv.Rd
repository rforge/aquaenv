\name{convert.aquaenv}
\alias{convert.aquaenv}
\title{convert.aquaenv}
\description{PRIVATE function: converts all elements of a special unit or pH scale in an object of class aquaenv}
\usage{convert.aquaenv(aquaenv, from, to, factor, convattr="unit", ...)}
\arguments{
  \item{aquaenv }{object of class aquaenv}
  \item{from }{the unit which needs to be converted (as a string; must be a perfect match)}
  \item{to }{the unit to which the conversion should go}
  \item{factor }{the conversion factor to be applied: can either be a number (e.g. 1000 to convert from mol to mmol), or any of the conversion factors given in an object of class  aquaenv}
  \item{convattr }{which attribute should be converted? can either be "unit" or "pH scale"}
  \item{... }{further arguments will be passed}
}
\value{
  object of class aquaenv whith the converted elements
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\keyword{misc}

