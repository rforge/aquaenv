\name{K_HCO3}
\alias{K_HCO3}
\title{K\_HCO3}
\description{PUBLIC function: calculates the dissociation constant of HCO3}
\usage{K_HCO3(S, t, p=0, SumH2SO4=NULL, SumHF=NULL, k1k2="roy")}
\arguments{
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{t }{temperature in degrees centigrade}
  \item{p }{gauge pressure (total pressure minus atmospheric pressure) in bars}
  \item{SumH2SO4 }{total sulfate concentration in mol/kg-solution (calculated from S if not supplied)}
  \item{SumHF }{total fluoride concentration in mol/kg-solution
    (calculated from S if not supplied)}
  \item{k1k2 }{"roy", "lueker", or "millero": specifies the S, t,
    dependency to be used. Default is "roy". (see section below for references)}
}
\value{
  the dissociation constant of HCO3 in mol/kg-solution on the free proton pH scale
}
\examples{
K_HCO3(35, 15)
K_HCO3(35, 15, 10)
K_HCO3(S=35, t=15, p=10, SumH2SO4=0.03)
plot(K_HCO3(35, 1:25), xlab="temperature / degC")
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{k1k2 = "roy": Roy1993b, DOE1994, Millero1995, Zeebe2001;
  k1k2 = "lueker": Lueker2000;
  k1k2 = "millero": Millero2006}
\keyword{misc}

