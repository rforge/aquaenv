\name{K_SiOH4}
\alias{K_SiOH4}
\title{K\_SiOH4}
\description{PUBLIC function: calculates the dissociation constant of SiOH4}
\usage{K\_SiOH4(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)}
\arguments{
  \item{Tc }{temperature in degrees centigrade}
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{d }{depth in meters}
  \item{SumH2SO4 }{total sulfate concentration in mol/kg-solution (calculated from S if not supplied)}
  \item{SumHF }{total fluoride concentration in mol/kg-solution (calculated from S if not supplied)}
}
\value{
  the dissociation constant of SiOH4 in mol/kg-solution on the free proton pH scale
}
\author{Andreas F. Hofmann}
\references{Millero1988, DOE1994, Millero1995}
\keyword{misc}

