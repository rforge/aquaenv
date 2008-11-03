\name{titration}
\alias{titration}
\title{titration}
\description{PUBLIC function: creates an object of class aquaenv which contains a titration simulation}
\usage{titration(aquaenv, mass_sample, mass_titrant, conc_titrant, S_titrant=NULL, steps, type)}
\arguments{
  \item{aquaenv }{an object of type aquaenv: minimal definition, contains all information about the system: T, S, d, total concentrations of nutrients etc}
  \item{mass_sample}{the mass of the sample solution in kg}
  \item{mass_titrant}{the total mass of the added titrant solution in kg}
  \item{conc_titrant}{the concentration of the titrant solution in mol/kg-soln}
  \item{S_titrant}{the salinity of the titrant solution, if not supplied it is assumed that the titrant solution has the same salinity as the sample solution}
  \item{steps}{the amount of steps the mass of titrant is added in} 
  \item{type}{the type of titrant: either "HCl" or "NaOH"}  
}
\value{
  object of class aquaenv which contains a titration simulation
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\keyword{misc}
