\name{K_HF}
\alias{K_HF}
\title{K\_HF}
\description{PUBLIC function: calculates the dissociation constant of HF}
\usage{K_HF(Tc, S, d=0)}
\arguments{
  \item{Tc }{temperature in degrees centigrade}
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{d }{depth in meters}
}
\value{
  the dissociation constant of HF in mol/kg-solution on the free proton pH scale
}
\examples{
K_HF(15, 35)
K_HF(15, 35, 10)
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{Dickson1979a, Dickson1987, Roy1993b, DOE1994, Millero1995,
  Zeebe2001
(Note that for a constant calculated from Perez1987, one needs to use
either aquaenv(khf="perez")\$K\_HF or seacarb)}
\keyword{misc}

