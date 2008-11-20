\name{scaleconvert}
\alias{scaleconvert}
\title{scaleconvert}
\description{PRIVATE function: provides pH scale conversion factors (caution: the activity coefficient for H+ (needed for NBS scale conversions) is calculated with the Davies equation (Zeebe2001) which is only accurate up to ionic strengthes of I = 0.5)}
\usage{scaleconvert(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL, khf="dickson")}
\arguments{
  \item{Tc }{temperature in degrees centigrade}
  \item{S }{salinity S in practical salinity units (i.e. no unit)}
  \item{d }{depth in meters}
  \item{SumH2SO4 }{total sulfate concentration in mol/kg-solution; if not supplied this is calculated from S}
  \item{SumHF }{total fluoride concentration in mol/kg-solution; if not
    supplied this is calculated from S}
  \item{khf}{either "dickson" (default, Dickson1979a) or "perez" (using seacarb, Perez1987a) for K\_HF}
}
\value{
  a list of conversion factors "free2tot", "free2sws", etc.
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\references{Dickson1984, DOE1994, Zeebe2001}
\keyword{misc}

