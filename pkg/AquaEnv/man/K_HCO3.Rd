\name{K_HCO3}
\alias{K_HCO3}
\title{K\_HCO3}
\description{PUBLIC function: calculates the dissociation constant of HCO3}
\usage{K_HCO3(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)}
\arguments{
  \item{Tc }{temperature in degrees centigrade}
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{d }{depth in meters}
  \item{SumH2SO4 }{total sulfate concentration in mol/kg-solution (calculated from S if not supplied)}
  \item{SumHF }{total fluoride concentration in mol/kg-solution (calculated from S if not supplied)}
}
\value{
  the dissociation constant of HCO3 in mol/kg-solution on the free proton pH scale
}
\examples{
K_HCO3(15, 35)
K_HCO3(15, 35, 10)
K_HCO3(Tc=15, S=35, d=10, SumH2SO4=0.03)
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{Roy1993b, DOE1994, Millero1995, Zeebe2001
(Note that for a constant calculated from Lueker2000, one needs to use
either aquaenv(k1k2="lueker")\$K\_HCO3 or seacarb)}
\keyword{misc}

