\name{selectplot}
\alias{selectplot}
\title{selectplot}
\description{PRIVATE function: plots just the elements of an object of class aquaenv given in what; no return value, just side-effect}
\usage{selectplot(aquaenv, xval, what, mfrow=c(1,1), size=c(7,7), ...)}
\arguments{
  \item{aquaenv }{object of class aquaenv}
  \item{xval }{x-value: the independent variable describing a change in elements of an object of class aquaenv}
  \item{what }{vector of names of elements of aquaenv that should be plotted}
  \item{mfrow }{standard plot parameter; default: just one plot}
  \item{size }{the size of the plot device; default: 7 (width) by 7 (height) inches}
  \item{... }{further arguments will be passed}
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\keyword{IO}

