\name{titration}
\alias{titration}
\title{titration}
\description{PUBLIC function: creates an object of class aquaenv which contains a titration simulation}
\usage{titration(aquaenv, mass_sample, mass_titrant, conc_titrant, S_titrant=NULL, steps, type, K1=NULL, seawater_titrant=FALSE)}
\arguments{
  \item{aquaenv }{an object of type aquaenv: minimal definition, contains all information about the system: T, S, d, total concentrations of nutrients etc (Note that it is possible to give values for SumBOH4, SumHSO4, and SumHF in the sample other than the ones calculated from salinity)}
  \item{mass_sample}{the mass of the sample solution in kg}
  \item{mass_titrant}{the total mass of the added titrant solution in kg}
  \item{conc_titrant}{the concentration of the titrant solution in mol/kg-soln}
  \item{S_titrant}{the salinity of the titrant solution, if not supplied it is assumed that the titrant solution has the same salinity as the sample solution}
  \item{steps}{the amount of steps the mass of titrant is added in} 
  \item{type}{the type of titrant: either "HCl" or "NaOH"}
  \item{K1}{used for TA fitting: give a K\_CO2 and NOT calculate it from T and S: i.e. K\_CO2 can be fitted in the routine as well}
  \item{seawater_titrant}{is the titrant based on natural seawater? (does it contain SumBOH4, SumHSO4, and SumHF in the same proportions as seawater, i.e., correlated to S?); Note that you can only assume a seawater based titrant (i.e. SumBOH4, SumHSO4, and SumHF ~ S) or a water based titrant (i.e. SumBOH4, SumHSO4, and SumHF = 0). It is not possible to give values for SumBOH4, SumHSO4, and SumHF of the titrant.}
}
\value{
  object of class aquaenv which contains a titration simulation
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\keyword{misc}
