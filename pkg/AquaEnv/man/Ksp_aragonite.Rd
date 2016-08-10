\name{Ksp_aragonite}
\alias{Ksp_aragonite}
\title{Ksp\_aragonite}
\description{PUBLIC function: calculates the solubility product for aragonite}
\usage{Ksp_aragonite(S, t, p=0)}
\arguments{
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{t }{temperature in degrees centigrade}
  \item{p }{gauge pressure (total pressure minus atmospheric pressure) in bars}
}
\value{
  the solubility product for aragonite in (mol/kg-solution)2
}
\examples{
Ksp_aragonite(35, 15)
Ksp_aragonite(35, 15, 10)
plot(Ksp_aragonite(35, 1:25), xlab="temperature / degC")
}
\author{Andreas F. Hofmann. Maintained by Karline Soetaert (Karline.Soetaert@nioz.nl).}
\references{Mucci1983, Boudreau1996}
\keyword{misc}

