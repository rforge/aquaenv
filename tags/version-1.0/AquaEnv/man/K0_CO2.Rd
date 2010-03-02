\name{K0_CO2}
\alias{K0_CO2}
\title{K0\_CO2}
\description{PUBLIC function: calculates the Henry's constant (solubility) for CO2}
\usage{K0_CO2(S, t)}
\arguments{
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{t }{temperature in degrees centigrade}
}
\value{
  the Henry's constant for CO2 in mol/(kg-solution*atm)
}
\examples{
K0_CO2(35, 15)
plot(K0_CO2(35, 1:25), xlab="temperature / degC")
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{Weiss1974, DOE1994, Millero1995, Zeebe2001}
\keyword{misc}

