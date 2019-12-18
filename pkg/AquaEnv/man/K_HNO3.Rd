\name{K_HNO3}
\alias{K_HNO3}
\alias{K_HNO2}
\alias{K_H2SO4}
\alias{K_HS}
\title{dissociation constants of HNO3, HNO2, H2SO4, HS}
\description{PUBLIC function: dissociation constants that are taken invariant}
\usage{K_HNO3(...)
K_HNO2(...)
K_H2SO4(...)
K_HS(...)
}
\arguments{
  \item{... }{not used, included for consistency with other dissociation constantss}
}
\value{
  the dissociation constant of HNO3, HNO2, H2SO4 and HS in mol/kg-solution on the free proton pH scale.
  
  These dissociation constants are taken as a constant, i.e. they do not vary with temperature nor with salinity.
}
\examples{
K_H2SO4(35, 15)
K_H2SO4()
}
\author{Karline Soetaert (Karline.Soetaert@nioz.nl).}
\references{K_HNO2: mol/l, NBS pH scale, hybrid constant (Riordan2005)

K_HNO3: assumed on mol/kg-soln and free pH scale, stoichiometric constant

K_H2SO4: assumed on mol/kg-soln and free pH scale, stoichiometric constant (Atkins1996)

K_HS: assumed on mol/kg-soln and free pH scale, stoichiometric constant (Atkins1996)}
\keyword{misc}

