\name{calcSumCO2_pH_TA}
\alias{calcSumCO2_pH_TA}
\title{calcSumCO2\_pH\_TA}
\description{PRIVATE function: calculates [SumCO2]  from an object of class aquanenv, a given pH, and a given [TA]: by analytically solving the resulting quadratic equation}
\usage{calcSumCO2\_pH\_TA(aquaenv, pH, TA)}
\arguments{
  \item{aquaenv }{object of class aquaenv}
  \item{pH }{given pH on the free proton scale}
  \item{TA }{given [TA] in mol/kg-solution}
}
\value{
  calculated [SumCO2] in mol/kg-solution
}
\author{Andreas F. Hofmann}
\keyword{misc}

