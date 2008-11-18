\name{K0_O2}
\alias{K0_O2}
\title{K0\_O2}
\description{PUBLIC function: calculates the Henry's constant (solubility) for O2}
\usage{K0_O2(Tc, S)}
\arguments{
  \item{Tc }{temperature in degrees centigrade}
  \item{S }{salinity in practical salinity units (i.e. no unit)}
}
\value{
  the Henry's constant for CO2 in mol/(kg-solution*atm)
}
\examples{K0_O2(15, 35)}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{derived from a formulation for [O2]sat given in Weiss1970}
\keyword{misc}

