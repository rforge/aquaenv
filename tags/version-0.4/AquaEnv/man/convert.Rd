\name{convert}
\alias{convert}
\title{convert}
\description{PUBLIC function: converts either a single value (the pH scale of a pH value, the pH scale of a dissociation constant (K*), the unit of a concentration value), or all elements of a special unit or pH scale in an object of class aquaenv}
\details{Possible usages are
\preformatted{
convert(x, vartype, what, Tc, S, d, SumH2SO4, SumHF)
convert(x, from, to, factor, convattr)}}
\arguments{
  \item{x }{object to be converted: either a single value (pH value, K* value, or concentration value) or an object of class aquaenv}
  \item{vartype }{only valid if x is a single value: the type of x, either "pHscale", "KHscale", or "conc"}
  \item{what }{only valid if x is a single value: only valid if x is a single value: the type of conversion to be done, for pH scales one of "free2tot", "free2sws", "free2nbs", ... (any combination of "free", "tot", "sws", and "nbs"); for concentrations one of "molar2molal", "molar2molin", ... (any combination of "molar" (mol/l), "molal" (mol/kg-H2O), and "molin" (mol/kg-solution))}
  \item{Tc }{only valid if x is a single value: temperature in degrees centigrade}
  \item{S }{only valid if x is a single value: salinity (in practical salinity units: no unit)}
  \item{d }{only valid if x is a single value: depth in meters}
  \item{SumH2SO4 }{only valid if x is a single value: total sulfate concentration in mol/kg-solution; if not supplied this is calculated from S}
  \item{SumHF }{only valid if x is a single value: total fluoride concentration in mol/kg-solution; if not supplied this is calculated from S}
  \item{from }{only valid if x is an object of class aquaenv: the unit which needs to be converted (as a string; must be a perfect match)}
  \item{to }{only valid if x is an object of class aquaenv: the unit to which the conversion should go}
  \item{factor }{only valid if x is an object of class aquaenv: the conversion factor to be applied: can either be a number (e.g. 1000 to convert from mol to mmol), or any of the conversion factors given in an object of class  aquaenv}
  \item{convattr }{only valid if x is an object of class aquaenv: which attribute should be converted? can either be "unit" or "pH scale"}
}
\value{
  converted single value or object of class aquaenv with converted elements
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\keyword{misc}

