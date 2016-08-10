\name{watdepth}
\alias{watdepth}
\title{watdepth}
\description{PUBLIC function: calculates the depth (in m) from the gauge
  pressure p (or the total pressure P) and the latitude (in degrees: -90
  to 90) and the atmospheric pressure Pa (in bar)}
\usage{watdepth(P=Pa, p=pmax(0, P-Pa), lat=0, Pa=1.013253)}
\arguments{
  \item{P }{total pressure in bar, standard: 1 atm (at the sea surface)}
  \item{p }{gauge pressure in bar (total pressure minus atmospheric pressure), standard: 0 (at the water surface)}
  \item{lat }{latitude in degrees: -90 to 90, standard: 0}
  \item{Pa }{atmospheric pressure in bar, standard: 1 atm (at sea level)}
}
\value{water depth d in meters}
\examples{
watdepth(100)
plot(watdepth(1:100))
}
\author{Andreas F. Hofmann. Maintained by Karline Soetaert (Karline.Soetaert@nioz.nl).}
\references{Fofonoff1983}
\keyword{misc}

