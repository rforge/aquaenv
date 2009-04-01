\name{K_H2S}
\alias{K_H2S}
\title{K\_H2S}
\description{PUBLIC function: calculates the dissociation constant of H2S}
\usage{K_H2S(S, t, p=0, SumH2SO4=NULL, SumHF=NULL)}
\arguments{
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{t }{temperature in degrees centigrade}
  \item{p }{gauge pressure (total pressure minus atmospheric pressure) in bars}
  \item{SumH2SO4 }{total sulfate concentration in mol/kg-solution (calculated from S if not supplied)}
  \item{SumHF }{total fluoride concentration in mol/kg-solution (calculated from S if not supplied)}
}
\value{
  the dissociation constant of H2S in mol/kg-solution on the free proton pH scale
}
\examples{
K_H2S(35, 15)
K_H2S(35, 15, 10)
K_H2S(S=35, t=15, p=10, SumH2SO4=0.03)
plot(K_H2S(35, 1:25), xlab="temperature / degC")
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{Millero1988, Millero1995}
\keyword{misc}

