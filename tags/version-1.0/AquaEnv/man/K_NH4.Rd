\name{K_NH4}
\alias{K_NH4}
\title{K\_NH4}
\description{PUBLIC function: calculates the dissociation constant of NH4}
\usage{K_NH4(S, t, p=0, SumH2SO4=NULL, SumHF=NULL, khf="dickson", khso4="dickson")}
\arguments{
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{t }{temperature in degrees centigrade}
  \item{p }{gauge pressure (total pressure minus atmospheric pressure) in bars}
  \item{SumH2SO4 }{total sulfate concentration in mol/kg-solution (calculated from S if not supplied)}
  \item{SumHF }{total fluoride concentration in mol/kg-solution
    (calculated from S if not supplied)}
   \item{khf}{S, t relation for K\_HF needed for scale conversion: either "dickson" (default, Dickson1979a) or "perez" (Perez1987a)}
  \item{khso4}{S, t relation for K\_HSO4 needed for scale conversion: either "dickson" (default, Dickson1990) or "khoo" (Khoo1977)}
}
\value{
  the dissociation constant of NH4 in mol/kg-solution on the free proton pH scale
}
\examples{
K_NH4(35, 15)
K_NH4(35, 15, 10)
K_NH4(S=35, t=15, p=10, SumH2SO4=0.03)
plot(K_NH4(35, 1:25), xlab="temperature / degC")
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{Millero1995a, Millero1995, corrected by Lewis1998}
\keyword{misc}

