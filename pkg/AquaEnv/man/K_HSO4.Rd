\name{K_HSO4}
\alias{K_HSO4}
\title{K\_HSO4}
\description{PUBLIC function: calculates the dissociation constant of HSO4}
\usage{K_HSO4(S, t, p=0)}
\arguments{
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{t }{temperature in degrees centigrade}
  \item{p }{gauge pressure (total pressure minus atmospheric pressure) in bars}
}
\value{
  the dissociation constant of HSO4 in mol/kg-solution on the free proton pH scale
}
\examples{
K_HSO4(35, 15)
K_HSO4(35, 15, 10)
plot(K_HSO4(35, 1:25), xlab="temperature / degC")
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{Dickson1990, DOE1994, Zeebe2001}
\keyword{misc}

