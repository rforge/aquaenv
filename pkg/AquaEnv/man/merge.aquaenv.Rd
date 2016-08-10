\name{merge.aquaenv}
\alias{merge.aquaenv}
\title{merge.aquaenv}
\description{PRIVATE function: merges the elements of two objects of class aquaenv: element names are taken from the first argument, the elements of which are also first in the merged object}
\usage{\method{merge}{aquaenv}(x, y, ...)}
\arguments{
  \item{x }{object of class aquaenv: this is where the element names are taken from}
  \item{y }{object of class aquaenv: must contain at leas all the element (names) as aquaenv1, extra elements are ignored}
  \item{... }{further arguments will be passed}
}
\value{
  object of class aquaenv with merged elements
}
\author{Andreas F. Hofmann. Maintained by Karline Soetaert (Karline.Soetaert@nioz.nl).}
\keyword{misc}

