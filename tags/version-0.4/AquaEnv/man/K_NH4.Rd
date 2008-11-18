\name{K_NH4}
\alias{K_NH4}
\title{K\_NH4}
\description{PUBLIC function: calculates the dissociation constant of NH4}
\usage{K_NH4(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)}
\arguments{
  \item{Tc }{temperature in degrees centigrade}
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{d }{depth in meters}
  \item{SumH2SO4 }{total sulfate concentration in mol/kg-solution (calculated from S if not supplied)}
  \item{SumHF }{total fluoride concentration in mol/kg-solution (calculated from S if not supplied)}
}
\value{
  the dissociation constant of NH4 in mol/kg-solution on the free proton pH scale
}
\examples{
K_NH4(15, 35)
K_NH4(15, 35, 10)
K_NH4(Tc=15, S=35, d=10, SumH2SO4=0.03)
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{Millero1995a, Millero1995, corrected by Lewis1998}
\keyword{misc}

