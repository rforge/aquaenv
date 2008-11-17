\name{K_HSO4}
\alias{K_HSO4}
\title{K\_HSO4}
\description{PUBLIC function: calculates the dissociation constant of HSO4}
\usage{K_HSO4(Tc, S, d=0)}
\arguments{
  \item{Tc }{temperature in degrees centigrade}
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{d }{depth in meters}
}
\value{
  the dissociation constant of HSO4 in mol/kg-solution on the free proton pH scale
}
\examples{
K_HSO4(15, 35)
K_HSO4(15, 35, 10)
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{Dickson1990, DOE1994, Zeebe2001}
\keyword{misc}

