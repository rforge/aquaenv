\name{convert.standard}
\alias{convert.standard}
\title{convert.standard}
\description{PRIVATE function: converts either the pH scale of a pH value, the pH scale of a dissociation constant (K*), or the unit of a concentration value}
\usage{convert.standard(x, vartype, what, Tc, S, d=0, SumH2SO4=NULL,
                  SumHF=NULL, khf="dickson")}
\arguments{
  \item{x }{the object to be converted (pH value, K* value, or concentration value)}
  \item{vartype }{the type of x, either "pHscale", "KHscale", or "conc"}
  \item{what }{the type of conversion to be done, for pH scales one of "free2tot", "free2sws", "free2nbs", ... (any combination of "free", "tot", "sws", and "nbs"); for concentrations one of "molar2molal", "molar2molin", ... (any combination of "molar" (mol/l), "molal" (mol/kg-H2O), and "molin" (mol/kg-solution))}
  \item{Tc }{temperature in degrees centigrade}
  \item{S }{salinity (in practical salinity units: no unit)}
  \item{d }{depth in meters}
  \item{SumH2SO4 }{total sulfate concentration in mol/kg-solution; if not supplied this is calculated from S}
  \item{SumHF }{total fluoride concentration in mol/kg-solution; if not supplied this is calculated from S}
  \item{khf}{either "dickson" (default, Dickson1979a) or "perez" (using seacarb, Perez1987a) for K\_HF}
}
\value{
  converted pH, K*, or concentration value, attributed with the new unit/pH scale
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\keyword{misc}

