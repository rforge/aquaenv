\name{titration}
\alias{titration}
\title{titration}
\description{PUBLIC function: creates an object of class aquaenv which contains a titration simulation (changes in [Na] and [Cl] and therefore in S are assumed negligible (~ 5 mmol/kg wrt 500 mmol/kg))}
\usage{titration(aquaenv, volume, amount, steps, type)}
\arguments{
  \item{aquaenv }{an object of type aquaenv: minimal definition, contains all information about the system: T, S, d, total concentrations of nutrients etc}
  \item{volume }{the volume of the (theoretical) titration vessel in l}
  \item{amount }{the amount of titrant added in mol}
  \item{steps }{the amount of steps the amount of titrant is added in}
  \item{type }{the type of titrant: either "HCl" or "NaOH"}
}
\value{
  object of class aquaenv which contains a titration simulation
}
\author{Andreas F. Hofmann}
\keyword{misc}

