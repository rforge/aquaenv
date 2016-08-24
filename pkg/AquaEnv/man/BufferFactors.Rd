\name{BufferFactors}
\alias{BufferFactors}
\title{BufferFactors}
\description{PUBLIC function: calculates buffer factors describing the
 sensitivity of pH and concentrations of acid-base species to a change
 in ocean chemistry}
\usage{BufferFactors(ae = NULL, parameters = NA, species = c("SumCO2"),
                     k_w = NULL, k_co2 = NULL, k_hco3 = NULL, 
                     k_boh3 = NULL, k_hso4 = NULL,
                     k_hf = NULL, k1k2 = "lueker", 
                     khf = "dickson", khso4 = "dickson")}
\arguments{
  \item{ae}{an object of class 'aquaenv'. An error is produced in case
  an object is provided that is not of class 'aquaenv',} 
  \item{parameters}{a vector containing one or more of the following
  variables: "DIC" (mol/kg-soln), "TotNH3" (mol/kg-soln), "TotP" 
  (mol/kg-soln), "TotNO3" (mol/kg-soln), "TotNO2" (mol/kg-soln), 
  "TotS" (mol/kg-soln), "TotSi" (mol/kg-soln), "TB" (mol/kg-soln), 
  "TotF" (mol/kg-soln), "TotSO4" (mol/kg-soln), "sal" (-), "temp" 
  (deg C), "pres" (bar), "Alk" (mol/kg-soln). If a variable is not 
  supplied and no object of class 'aquaenv' is provided, default values 
  are assigned following Table 4 of Hagens and Middelburg (2016). If 
  both ae and parameters are supplied, given parameters will overwrite 
  the corresponding values of ae}
  \item{species}{a vector containing one or more of the following
  variables: "SumCO2", "SumNH4", "SumH3PO4", "SumHNO3", "SumHNO2", 
  "SumH2S", "SumSiOH4", "SumBOH3", "SumHF", "SumH2SO4", "CO2", "HCO3",            
  "CO3", "BOH3", "BOH4", "OH", "H3PO4", "H2PO4", "HPO4", "PO4", 
  "SiOH4", "SiOOH3", "SiO2OH2", "H2S", "HS", "S2min", "NH4", "NH3", 
  "H2SO4", "HSO4", "SO4", "HF", "F", "HNO3", "NO3", "HNO2", "NO2". 
  Default is c("SumCO2"). This vector defines the species for which the
  sensitivities are calculated}
  \item{k_w}{a fixed K\_W can be specified}
  \item{k_co2}{a fixed K\_CO2 can be specified}
  \item{k_hco3}{a fixed K\_HCO3 can be specified}
  \item{k_boh3}{a fixed K\_BOH3 can be specified}
  \item{k_hso4}{a fixed K\_HSO4 can be specified}
  \item{k_hf}{a fixed K\_HF can be specified}
  \item{k1k2}{either "lueker" (default, Lueker2000), "roy" (Roy1993a),
    or "millero" (Millero2006) for K\_CO2 and K\_HCO3}
  \item{khf}{either "dickson" (default, Dickson1979a) or "perez"
    (Perez1987a) for K\_HF}
  \item{khso4}{either "dickson" (default, Dickson1990) or "khoo" 
    (Khoo1977) for K\_HSO4}
}
\examples{\dontrun{
# Default run
BufferFactors()

# All carbonate system species
BufferFactors(species = c("CO2", "HCO3", "CO3"))

# Total concentrations of all species
BufferFactors(species = c("SumCO2", "SumNH4", "SumH3PO4", "SumHNO3",
                          "SumHNO2", "SumH2S", "SumSiOH4", "SumBOH3",
                          "SumHF", "SumH2SO4"))

# Different carbonate system equilibrium constants
BufferFactors(k1k2 = "roy")

# Object of class 'aquaenv' as input
ae_input <- aquaenv(S=35, t=25, SumCO2 = 0.0020, pH = 8.1,
                    skeleton = TRUE)
BufferFactors(ae = ae_input)

# Produces some NaNs as certain total concentrations are zero
BufferFactors(ae = ae_input, 
              species = c("SumCO2", "SumNH4", "SumH3PO4", "SumHNO3",
                          "SumHNO2", "SumH2S", "SumSiOH4", "SumBOH3",
                          "SumHF", "SumH2SO4"))

# Object of class 'aquaenv' as input, but different total alkalinity
parameters <- c(Alk = 0.0022)
BufferFactors(ae = ae_input, parameters = parameters)

}}

\value{a list containing the objects "ae", "dTA.dH", "dtotX.dH", 
  "dTA.dX", "dtotX.dX", "dTA.dpH", "dtotX.dpH", "dH.dTA", "dH.dtotX", 
  "dX.dTA", "dX.dtotX", "dpH.dTA", "dpH.dtotX", "beta.H" and "RF".
  
  The object 'ae' is of class 'aquaenv' and refers to the output of the 
  aquaenv function that is always run as part of BufferFactors. Consult 
  the vignette of AquaEnv for more information on this object. The 
  other objects are vectors with the length and names of the input 
  species. Exceptions here are dH.dtotX and dpH.dtotX, which also 
  contain the numerically estimated sensitivities with respect to 
  salinity, pressure and temperature, as well as two factors related to 
  pH scale conversion (see the AquaEnv vignette for details on these
  latter conversion factors).
  
  In case species are defined which corresponding total concentration 
  equals zero, the corresponding output produces 'NaN'. This is with
  the exception of "dTA.dH" and "dTA.dpH", which are always calculated
  as they are linked to beta.H. Additionally, the Revelle factor is
  always calculated, as the function 'aquaenv' requires that the 
  carbonate system be specified.
}

\references{
Hagens M. and J.J. Middelburg, 2016 Generalised expressions for the response of pH to changes in ocean chemistry. \emph{Geochimica et Cosmochimica Acta, in press}.
}
             
\author{Mathilde Hagens (\email{M.Hagens@uu.nl})}
\keyword{misc}

