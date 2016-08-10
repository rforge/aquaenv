\name{K0_O2}
\alias{K0_O2}
\title{K0\_O2}
\description{PUBLIC function: calculates the Henry's constant (solubility) for O2}
\usage{K0_O2(S, t)}
\arguments{
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{t }{temperature in degrees centigrade}
}
\value{
  the Henry's constant for CO2 in mol/(kg-solution*atm)
}
\examples{
K0_O2(35, 15)
plot(K0_O2(35, 1:25), xlab="temperature / degC")
}
\author{Andreas F. Hofmann. Maintained by Karline Soetaert (Karline.Soetaert@nioz.nl).}
\references{derived from a formulation for [O2]sat given in Weiss1970}
\keyword{misc}

