\name{K_SiOOH3}
\alias{K_SiOOH3}
\title{K\_SiOOH3}
\description{PUBLIC function: calculates the dissociation constant of SiOOH3}
\usage{K_SiOOH3(S, t, p=0, SumH2SO4=NULL, SumHF=NULL)}
\arguments{
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{t }{temperature in degrees centigrade}
  \item{p }{gauge pressure (total pressure minus atmospheric pressure) in bars}
  \item{SumH2SO4 }{total sulfate concentration in mol/kg-solution (calculated from S if not supplied)}
  \item{SumHF }{total fluoride concentration in mol/kg-solution (calculated from S if not supplied)}
}
\value{
  the dissociation constant of SiOOH3 in mol/kg-solution on the free proton pH scale
}
\examples{
K_SiOOH3(35, 15)
K_SiOOH3(35, 15, 10)
K_SiOOH3(S=35, t=15, p=10, SumH2SO4=0.03)
plot(K_SiOOH3(35, 1:25), xlab="temperature / degC")}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{Wischmeyer2003 (incl. corrections)}
\keyword{misc}

