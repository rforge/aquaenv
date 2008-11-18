\name{titration}
\alias{titration}
\title{titration}
\description{PUBLIC function: creates an object of class aquaenv which contains a titration simulation}
\usage{titration(aquaenv, mass_sample, mass_titrant, conc_titrant,
          S_titrant=NULL, steps, type="HCl", seawater_titrant=FALSE,
          k_w=NULL, k_co2=NULL, k_hco3=NULL, k_boh3=NULL, k_hso4=NULL,
          k_hf=NULL)}
\arguments{
  \item{aquaenv }{an object of type aquaenv: minimal definition, contains all information about the system: T, S, d, total concentrations of nutrients etc (Note that it is possible to give values for SumBOH4, SumHSO4, and SumHF in the sample other than the ones calculated from salinity)}
  \item{mass_sample}{the mass of the sample solution in kg}
  \item{mass_titrant}{the total mass of the added titrant solution in kg}
  \item{conc_titrant}{the concentration of the titrant solution in mol/kg-soln}
  \item{S_titrant}{the salinity of the titrant solution, if not supplied it is assumed that the titrant solution has the same salinity as the sample solution}
  \item{steps}{the amount of steps the mass of titrant is added in} 
  \item{type}{the type of titrant: either "HCl" or "NaOH", default: "HCl"}
  \item{seawater_titrant}{is the titrant based on natural seawater?
    (does it contain SumBOH4, SumHSO4, and SumHF in the same proportions
    as seawater, i.e., correlated to S?); Note that you can only assume
    a seawater based titrant (i.e. SumBOH4, SumHSO4, and SumHF ~ S) or a
    water based titrant (i.e. SumBOH4, SumHSO4, and SumHF = 0). It is
    not possible to give values for SumBOH4, SumHSO4, and SumHF of the
    titrant.}
  \item{k_w}{a fixed K\_W can be specified}
  \item{k_co2}{a fixed K\_CO2 can be specified; used for TA fitting: give a K\_CO2 and NOT calculate it from T and S: i.e. K\_CO2 can be fitted in the routine as well}
  \item{k_hco3}{a fixed K\_HCO3 can be specified}
  \item{k_boh3}{a fixed K\_BOH3 can be specified}
  \item{k_hso4}{a fixed K\_HSO4 can be specified}
  \item{k_hf}{a fixed K\_HF can be specified}
}
\value{
  object of class aquaenv which contains a titration simulation
}
\examples{
####################
# Titration with HCl
####################
Tc <- 15
S  <- 35

SumCO2 <- 0.003500
SumNH4 <- 0.000020

mass_sample  <- 0.01 # the mass of the sample solution in kg
mass_titrant <- 0.02 # the total mass of the added titrant solution in
                     # kg
conc_titrant <- 0.01 # the concentration of the titrant solution in
                     # mol/kg-soln
S_titrant    <- 0.5  # the salinity of the titrant solution (the
                     # salinity of a solution with a ionic strength of
                     # 0.01 according to: I = (19.924 S) / (1000 - 1.005S)
steps        <- 50   # the amount of steps the mass of titrant is added
                     # in
type         <- "HCl"

pHstart <- 11.3


ae <- titration(aquaenv(Tc=Tc, S=S, SumCO2=SumCO2, SumNH4=SumNH4,
                pH=pHstart), mass_sample, mass_titrant, conc_titrant,
                S_titrant, steps, type)


# plotting everything
plot(ae, xval=ae$delta_mass_titrant, xlab="HCl solution added [kg]",
mfrow=c(10,10), newdevice=FALSE)


# plotting selectively
size  <- c(12,8) #inches
mfrow <- c(4,4)
what  <- c("TA", "pH", "CO2", "HCO3", "CO3", "BOH3", "BOH4", "OH",
           "NH4", "NH3", "H2SO4", "HSO4", "SO4", "HF", "F", "pCO2")


plot(ae, xval=ae$delta_mass_titrant, xlab="HCl solution added [kg]",
     what=what, size=size, mfrow=mfrow, newdevice=FALSE)

plot(ae, xval=ae$pH, xlab="free scale pH", what=what, size=size,
     mfrow=mfrow, newdevice=FALSE)


# different x values
plot(ae, xval=ae$delta_conc_titrant, xlab="[HCl] offset added
     [mol/kg-soln]", what=what, size=size, mfrow=mfrow, newdevice=FALSE)

plot(ae, xval=ae$delta_moles_titrant, xlab="HCl added [mol]", what=what,
     size=size, mfrow=mfrow, newdevice=FALSE)


# bjerrum plots
plot(ae, bjerrum=TRUE, newdevice=FALSE)

what  <- c("CO2", "HCO3", "CO3")
plot(ae, what=what, bjerrum=TRUE, newdevice=FALSE)
plot(ae, what=what, bjerrum=TRUE, lwd=4, palette=c("cyan", "magenta",
     "yellow"), bg="gray", legendinset=0.1, legendposition="topleft",
     newdevice=FALSE)



what  <- c("CO2", "HCO3", "CO3", "BOH3", "BOH4", "OH", "NH4", "NH3",
           "H2SO4", "HSO4", "SO4", "HF", "F")

plot(ae, what=what, bjerrum=TRUE, log=TRUE, newdevice=FALSE)
plot(ae, what=what, bjerrum=TRUE, log=TRUE, ylim=c(-6,-1),
     legendinset=0, lwd=3, palette=c(1,3,4,5,6,colors()[seq(100,250,6)]),
     newdevice=FALSE)
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\keyword{misc}
