\name{K0_CO2}
\alias{K0_CO2}
\title{K0\_CO2}
\description{PUBLIC function: calculates the Henry's constant (solubility) for CO2}
\usage{K0_CO2(Tc, S)}
\arguments{
  \item{Tc }{temperature in degrees centigrade}
  \item{S }{salinity in practical salinity units (i.e. no unit)}
}
\value{
  the Henry's constant for CO2 in mol/(kg-solution*atm)
}
\examples{
K0_CO2(15, 35)
plot(K0_CO2(1:25, 35), xlab="temperature / degC")
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{Weiss1974, DOE1994, Millero1995, Zeebe2001}
\keyword{misc}

