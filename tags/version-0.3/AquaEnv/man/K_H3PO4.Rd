\name{K_H3PO4}
\alias{K_H3PO4}
\title{K\_H3PO4}
\description{PUBLIC function: calculates the dissociation constant of H3PO4}
\usage{K_H3PO4(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)}
\arguments{
  \item{Tc }{temperature in degrees centigrade}
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{d }{depth in meters}
  \item{SumH2SO4 }{total sulfate concentration in mol/kg-solution (calculated from S if not supplied)}
  \item{SumHF }{total fluoride concentration in mol/kg-solution (calculated from S if not supplied)}
}
\value{
  the dissociation constant of H3PO4 in mol/kg-solution on the free proton pH scale
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{Millero1995 (original, SWS pH version), DOE1994 (in a later revision cites Millero1995)}
\keyword{misc}

