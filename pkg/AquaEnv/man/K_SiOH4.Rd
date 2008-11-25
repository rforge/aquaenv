\name{K_SiOH4}
\alias{K_SiOH4}
\title{K\_SiOH4}
\description{PUBLIC function: calculates the dissociation constant of SiOH4}
\usage{K_SiOH4(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)}
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
\examples{
K_SiOH4(15, 35)
K_SiOH4(15, 35, 10)
K_SiOH4(Tc=15, S=35, d=10, SumH2SO4=0.03)
plot(K_SiOH4(1:25, 35), xlab="temperature / degC")
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{Millero1988, DOE1994, Millero1995}
\keyword{misc}

