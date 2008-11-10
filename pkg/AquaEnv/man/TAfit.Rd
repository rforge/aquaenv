\name{TAfit}
\alias{TAfit}
\title{TAfit}
\description{PUBLIC function: calculates [TA] and [SumCO2] (and optionally K\_C02 and E0) from a titration curve using an optimization procedure (nls.lm from R package minpack.lm)}
\usage{TAfit(ae, titcurve, conc_titrant, mass_sample, S_titrant=NULL,
      TASumCO2guess=0.0025, E0guess=0.4, type="HCl", Evals=FALSE,
      K_CO2fit=FALSE,equalspaced=TRUE, seawater_titrant=FALSE,
      pHscale="free", debug=FALSE, 
      k_w=NULL, k_co2=NULL, k_hco3=NULL, k_boh3=NULL, k_hso4=NULL, k_hf=NULL,
      nlscontrol=nls.lm.control())}
\arguments{
  \item{ae }{an object of type aquaenv: minimal definition, contains all information about the system: T, S, d, total concentrations of nutrients etc (Note that it is possible to give values for SumBOH4, SumHSO4, and SumHF in the sample other than the ones calculated from salinity)}
  \item{titcurve }{a table containing the titration curve: basically a series of tuples of added titrant solution mass and pH values (pH on free proton scale) or E values in V}
  \item{conc_titrant }{concentration of the titrant solution in mol/kg-soln}
  \item{mass_sample }{the mass of the sample solution in kg}
  \item{S_titrant }{the salinity of the titrant solution, if not supplied it is assumed that the titrant solution has the same salinity as the sample solution}
  \item{TASumCO2guess }{a first guess for [TA] and [SumCO2] to be used as initial values for the optimization procedure}
  \item{E0guess }{first guess for E0 in V}
  \item{type }{the type of titrant: either "HCl" or "NaOH"}
  \item{Evals }{are the supplied datapoints pH or E (V) values?}
  \item{K_CO2fit }{should K\_CO2 be fitted as well?}
  \item{equalspaced }{are the mass values of titcurve equally spaced?}
  \item{seawater_titrant }{is the titrant based on  natural seawater? (does it contain SumBOH4, SumHSO4, and SumHF in the same proportions as seawater, i.e., correlated to S?); Note that you can only assume a seawater based titrant (i.e. SumBOH4, SumHSO4, and SumHF ~ S) or a water based titrant (i.e. SumBOH4, SumHSO4, and SumHF = 0). It is not possible to give values for SumBOH4, SumHSO4, and SumHF of the titrant.}
  \item{pHscale}{either "free", "total", "sws" or "nbs": if the titration curve contains pH data: on which scale is it measured?}
  \item{debug}{debug mode: the last simulated titration tit, the
    converted pH profile calc, and the nls.lm output out are made global
    variables for investigation and plotting}
  \item{k_w}{a fixed K\_W can be specified}
  \item{k_co2}{a fixed K\_CO2 can be specified; used for TA fitting: give a K\_CO2 and NOT calculate it from T and S: i.e. K\_CO2 can be fitted in the routine as well}
  \item{k_hco3}{a fixed K\_HCO3 can be specified}
  \item{k_boh3}{a fixed K\_BOH3 can be specified}
  \item{k_hso4}{a fixed K\_HSO4 can be specified}
  \item{k_hf}{a fixed K\_HF can be specified}
  \item{nlscontrol}{nls.lm.control() can be specified}
}
\value{
  a list of up to five values ([TA] in mol/kg-solution, [SumCO2] in mol/kg-solution, E0 in V, K1 in mol/kg-solution and on free scale, sum of the squared residuals)
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\keyword{misc}


