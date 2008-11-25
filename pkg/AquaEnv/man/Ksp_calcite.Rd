\name{Ksp_calcite}
\alias{Ksp_calcite}
\title{Ksp\_calcite}
\description{PUBLIC function: calculates the solubility product for aragonite}
\usage{Ksp_calcite(Tc, S, d=0)}
\arguments{
  \item{Tc }{temperature in degrees centigrade}
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{d }{depth in meters}
}
\value{
  the solubility product for calcite in (mol/kg-solution)2
}
\examples{
Ksp_calcite(15, 35)
Ksp_calcite(15, 35, 10)
plot(Ksp_aragonite(1:25, 35), xlab="temperature / degC")
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{Mucci1983, Boudreau1996}
\keyword{misc}

