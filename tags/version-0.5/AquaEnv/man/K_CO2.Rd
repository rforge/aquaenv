\name{K_CO2}
\alias{K_CO2}
\title{K\_CO2}
\description{PUBLIC function: calculates the dissociation constant of CO2}
\usage{K_CO2(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)}
\arguments{
  \item{Tc }{temperature in degrees centigrade}
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{d }{depth in meters}
  \item{SumH2SO4 }{total sulfate concentration in mol/kg-solution (calculated from S if not supplied)}
  \item{SumHF }{total fluoride concentration in mol/kg-solution (calculated from S if not supplied)}
}
\value{
  the dissociation constant of CO2 in mol/kg-solution on the free proton pH scale
}
\examples{
K_CO2(15, 35)
K_CO2(15, 35, 10)
K_CO2(Tc=15, S=35, d=10, SumH2SO4=0.03)
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{Roy1993b, DOE1994, Millero1995, Zeebe2001
(Note that for a constant calculated from Lueker2000, one needs to use
either aquaenv(k1k2="lueker")\$K\_CO2 or seacarb)}
\keyword{misc}

