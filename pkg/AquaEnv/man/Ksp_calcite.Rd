\name{Ksp_calcite}
\alias{Ksp_calcite}
\title{Ksp\_calcite}
\description{PUBLIC function: calculates the solubility product for aragonite}
\usage{Ksp_calcite(S, t, p=0)}
\arguments{
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{t }{temperature in degrees centigrade}
  \item{p }{gauge pressure (total pressure minus atmospheric pressure) in bars}
}
\value{
  the solubility product for calcite in (mol/kg-solution)2
}
\examples{
Ksp_calcite(35, 15)
Ksp_calcite(35, 15, 10)
plot(Ksp_aragonite(35, 1:25), xlab="temperature / degC")
}
\author{Andreas F. Hofmann. Maintained by Karline Soetaert (Karline.Soetaert@nioz.nl).}
\references{Mucci1983, Boudreau1996}
\keyword{misc}

