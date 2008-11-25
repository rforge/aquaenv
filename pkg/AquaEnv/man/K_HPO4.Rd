\name{K_HPO4}
\alias{K_HPO4}
\title{K\_HPO4}
\description{PUBLIC function: calculates the dissociation constant of HPO4}
\usage{K_HPO4(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)}
\arguments{
  \item{Tc }{temperature in degrees centigrade}
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{d }{depth in meters}
  \item{SumH2SO4 }{total sulfate concentration in mol/kg-solution (calculated from S if not supplied)}
  \item{SumHF }{total fluoride concentration in mol/kg-solution (calculated from S if not supplied)}
}
\value{
  the dissociation constant of HPO4 in mol/kg-solution on the free proton pH scale
}
\examples{
K_HPO4(15, 35)
K_HPO4(15, 35, 10)
K_HPO4(Tc=15, S=35, d=10, SumH2SO4=0.03)
plot(K_HPO4(1:25, 35), xlab="temperature / degC")
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{Millero1995 (original, SWS pH version), DOE1994 (in a later revision cites Millero1995)}
\keyword{misc}

