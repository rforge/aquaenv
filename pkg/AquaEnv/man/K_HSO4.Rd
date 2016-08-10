\name{K_HSO4}
\alias{K_HSO4}
\title{K\_HSO4}
\description{PUBLIC function: calculates the dissociation constant of HSO4}
\usage{K_HSO4(S, t, p=0, khso4="dickson")}
\arguments{
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{t }{temperature in degrees centigrade}
  \item{p }{gauge pressure (total pressure minus atmospheric pressure)
    in bars}
  \item{khso4}{"dickson" or "khoo": specifies the S, t,
    dependency to be used. Default is "dickson". (see section below for references)}
}
\value{
  the dissociation constant of HSO4 in mol/kg-solution on the free proton pH scale
}
\examples{
K_HSO4(35, 15)
K_HSO4(35, 15, 10)
plot(K_HSO4(35, 1:25), xlab="temperature / degC")
}
\author{Andreas F. Hofmann. Maintained by Karline Soetaert (Karline.Soetaert@nioz.nl).}
\references{"dickson": Dickson1990, DOE1994, Zeebe2001;
            "khoo":    Khoo1977, Roy1993, Millero1995}
\keyword{misc}

