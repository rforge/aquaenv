\name{gauge_p}
\alias{gauge_p}
\title{gauge\_p}
\description{PUBLIC function: calculates the gauge pressure (total pressure minus atmospheric pressure) from the depth (in m) and the latitude (in degrees: -90 to 90) and the atmospheric pressure (in bar)}
\usage{gauge_p(d, lat=0, Pa=1.01325)}
\arguments{
  \item{d }{water depth in meters}
  \item{lat }{latitude in degrees: -90 to 90, standard: 0}
  \item{Pa }{atmospheric pressure in bar, standard: 1 atm (at sea level)}
}
\value{gauge pressure (total pressure minus atmospheric pressure) p in bars}
\examples{
gauge_p(100)
plot(gauge_p(1:100))
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{Fofonoff1983}
\keyword{misc}

