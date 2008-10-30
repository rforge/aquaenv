\name{plot.aquaenv}
\alias{plot.aquaenv}
\title{plot.aquaenv}
\description{PUBLIC function: high level plot function for objects of class aquaenv}
\usage{plot.aquaenv(x, xval, what=NULL, bjerrum=FALSE, cumulative=FALSE, ...)}
\arguments{
  \item{x}{object of class aquaenv}
  \item{xval }{a vector of the (maximal) length of the elements of aquaenv against which they are to be plotted}
  \item{what }{a list of names of the elements of aquaenv that are to be plotted, if not supplied and bjerrum=FALSE and cumulative=FALSE: all elements are plotted, if not supplied and bjerrum=TRUE then what is set to be c("CO2", "HCO3", "CO3", "BOH3", "BOH4", "OH", "H3PO4", "H2PO4", "HPO4", "PO4", "SiOH4", "SiOOH3", "SiO2OH2", "H2S", "HS", "S2min", "NH4", "NH3", "H2SO4", "HSO4", "SO4", "HF", "F", "HNO3", "NO3", "HNO2", "NO2"), needs to be supplied for cumulative=TRUE}
  \item{bjerrum }{flag: TRUE = a bjerrum plot is done (by calling bjerrumplot)}
  \item{cumulative }{flag: TRUE = a cumulative plot is done (by calling cumulativeplot)}
  \item{... }{further arguments are passed on: to plotall, selectplot, bjerrumplot, cumulativeplot}	
  }
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\keyword{misc}

