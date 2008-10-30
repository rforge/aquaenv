\name{TAfit}
\alias{TAfit}
\title{TAfit}
\description{PUBLIC function: calculates [TA] and [SumCO2] from a titration curve using an optimization procedure (nls.lm from R package minpack.lm)}
\usage{TAfit(ae, pHmeasurements, volume, amount, TAguess=0.0025, type="HCl")}
\arguments{
  \item{ae }{an object of type aquaenv: minimal definition, contains all information about the system: T, S, d, total concentrations of nutrients etc}
  \item{pHmeasurements }{a table containing the titration curve: basically a series of pH values (pH on free proton scale)}
  \item{volume }{the volume of the titration vessel}
  \item{amount }{the total amount of the titrant added}
  \item{TAguess }{a first guess for [TA] and [SumCO2] to be used as initial values for the optimization procedure}
  \item{type }{the type of titrant: either "HCl" or "NaOH"}
}
\value{
  a list of three values ([TA] in mol/kg-solution, [SumCO2] in mol/kg-solution, sum of the squared residuals)
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\keyword{misc}

