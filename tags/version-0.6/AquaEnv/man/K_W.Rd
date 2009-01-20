\name{K_W}
\alias{K_W}
\title{K\_W}
\description{PUBLIC function: calculates the ion product of H2O}
\usage{K_W(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)}
\arguments{
  \item{Tc }{temperature in degrees centigrade}
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{d }{depth in meters}
  \item{SumH2SO4 }{total sulfate concentration in mol/kg-solution (calculated from S if not supplied)}
  \item{SumHF }{total fluoride concentration in mol/kg-solution (calculated from S if not supplied)}
}
\value{
  the ion product of H2O in (mol/kg-solution)2 on the free proton pH scale
}
\examples{
K_W(15, 35)
K_W(15, 35, 10)
K_W(Tc=15, S=35, d=10, SumH2SO4=0.03)
plot(K_W(1:25, 35), xlab="temperature / degC")
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{Millero1995 (SWS pH version), DOE1994 (cites Millero1995), Zeebe2001}
\keyword{misc}

