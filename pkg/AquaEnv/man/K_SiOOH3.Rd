\name{K_SiOOH3}
\alias{K_SiOOH3}
\title{K\_SiOOH3}
\description{PUBLIC function: calculates the dissociation constant of SiOOH3}
\usage{K_SiOOH3(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)}
\arguments{
  \item{Tc }{temperature in degrees centigrade}
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{d }{depth in meters}
  \item{SumH2SO4 }{total sulfate concentration in mol/kg-solution (calculated from S if not supplied)}
  \item{SumHF }{total fluoride concentration in mol/kg-solution (calculated from S if not supplied)}
}
\value{
  the dissociation constant of SiOOH3 in mol/kg-solution on the free proton pH scale
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{Wischmeyer2003 (incl. corrections)}
\keyword{misc}

