\name{K_CO2}
\alias{K_CO2}
\title{K\_CO2}
\description{PUBLIC function: calculates the dissociation constant of CO2}
\usage{K_CO2(S, t, p=0, SumH2SO4=NULL, SumHF=NULL, k1k2="roy", khf="dickson", khso4="dickson")}
\arguments{
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{t }{temperature in degrees centigrade}
  \item{p }{gauge pressure (total pressure minus atmospheric pressure) in bars}
  \item{SumH2SO4 }{total sulfate concentration in mol/kg-solution (calculated from S if not supplied)}
  \item{SumHF }{total fluoride concentration in mol/kg-solution
    (calculated from S if not supplied)}
  \item{k1k2 }{"roy", "lueker", or "millero": specifies the S, t,
    dependency to be used. Default is "roy". (see section below for
    references)}
  \item{khf}{S, t relation for K\_HF needed for scale conversion: either "dickson" (default, Dickson1979a) or "perez" (Perez1987a)}
  \item{khso4}{S, t relation for K\_HSO4 needed for scale conversion: either "dickson" (default, Dickson1990) or "khoo" (Khoo1977)}
}
\value{
  the dissociation constant of CO2 in mol/kg-solution on the free proton pH scale
}
\examples{
K_CO2(35, 15)
K_CO2(35, 15, 10)
K_CO2(S=35, t=15, p=10, SumH2SO4=0.03)
plot(K_CO2(35, 1:25), xlab="temperature / degC")
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{k1k2 = "roy": Roy1993b, DOE1994, Millero1995, Zeebe2001;
  k1k2 = "lueker": Lueker2000;
  k1k2 = "millero": Millero2006}
\keyword{misc}

