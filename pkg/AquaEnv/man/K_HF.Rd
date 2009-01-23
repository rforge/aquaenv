\name{K_HF}
\alias{K_HF}
\title{K\_HF}
\description{PUBLIC function: calculates the dissociation constant of HF}
\usage{K_HF(S, t, p=0)}
\arguments{
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{t }{temperature in degrees centigrade}
  \item{p }{gauge pressure (total pressure minus atmospheric pressure) in bars}
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
\references{Dickson1979a, Dickson1987, Roy1993b, DOE1994, Millero1995,
  Zeebe2001
(Note that for a constant calculated from Perez1987, one needs to use
either aquaenv(khf="perez")\$K\_HF or seacarb)}
\keyword{misc}

