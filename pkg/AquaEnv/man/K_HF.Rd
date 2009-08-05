\name{K_HF}
\alias{K_HF}
\title{K\_HF}
\description{PUBLIC function: calculates the dissociation constant of HF}
\usage{K_HF(S, t, p=0, SumH2SO4=NULL, SumHF=NULL, khf="dickson", khso4="dickson")}
\arguments{
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{t }{temperature in degrees centigrade}
  \item{p }{gauge pressure (total pressure minus atmospheric pressure)
    in bars}
  \item{SumH2SO4 }{total sulfate concentration in mol/kg-solution (calculated from S if not supplied)}
  \item{SumHF }{total fluoride concentration in mol/kg-solution
    (calculated from S if not supplied)}
  \item{khf }{"dickson" or "perez": specifies the S, t,
    dependency to be used. Default is "dickson". (see section below for
    references)}
  \item{khso4}{S, t relation for K\_HSO4 needed for scale conversion: either "dickson" (default, Dickson1990) or "khoo" (Khoo1977)}
}
\value{
  the dissociation constant of HF in mol/kg-solution on the free proton pH scale
}
\examples{
K_HF(35, 15)
K_HF(35, 15, 10)
plot(K_HF(35, 1:25), xlab="temperature / degC")
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{khf = "dickson": Dickson1979a, Dickson1987, Roy1993b, DOE1994, Millero1995,
  Zeebe2001;
khf = "perez": Perez1987}
\keyword{misc}

