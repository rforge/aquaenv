\name{seaconc}
\alias{seaconc}
\title{seaconc}
\description{PRIVATE function: calculates concentrations of constituents of natural seawater from a given salinity S}
\usage{seaconc(spec, S)}
\arguments{
  \item{spec }{constituent of seawater (chemical species) of which the concentration should be calculated. can be any name of the vectors ConcRelCl and MeanMolecularWeight: "Cl", "SO4", "Br", "F", "Na", "Mg", "Ca", "K", "Sr", "B", "S"}
  \item{S }{salinity S in practical salinity units (i.e. no unit)}
}
\value{
  concentration of the constituent of seawater speciefied in spec in mol/kg-solution (molinity): this is determined by the data in ConcRelCl and MeanMolecularWeight
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{DOE1994}
\keyword{misc}

