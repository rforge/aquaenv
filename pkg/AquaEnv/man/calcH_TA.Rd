\name{calcH_TA}
\alias{calcH_TA}
\title{calcH\_TA}
\description{PRIVATE function: calculates [H+]  from an object of class aquanenv and a given [TA]: first according to Follows2006, if no solution is found after Technicals$maxiter iterations, uniroot is applied}
\usage{calcH\_TA(aquaenv, TA)}
\arguments{
  \item{aquaenv }{object of class aquaenv}
  \item{TA }{given [TA] in mol/kg-solution}
}
\value{
  calculated [H+] in mol/kg-solution
}
\author{Andreas F. Hofmann}
\keyword{misc}

