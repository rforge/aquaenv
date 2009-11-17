\name{TAfit}
\alias{TAfit}
\title{TAfit}
\description{PUBLIC function: calculates [TA] and [SumCO2] (and optionally K\_C02 and E0) from a titration curve using an optimization procedure (nls.lm from R package minpack.lm)}
\usage{TAfit(ae, titcurve, conc_titrant, mass_sample, S_titrant=NULL,
      TASumCO2guess=0.0025, E0guess=0.4, type="HCl", Evals=FALSE,
      electrode_polarity="pos", K_CO2fit=FALSE,
      equalspaced=TRUE, seawater_titrant=FALSE,
      pHscale="free", debug=FALSE, k_w=NULL, k_co2=NULL, k_hco3=NULL,
      k_boh3=NULL, k_hso4=NULL, k_hf=NULL,
      nlscontrol=nls.lm.control(), verbose=FALSE,
      k1k2="roy", khf="dickson", datxbegin=0, SumCO2Zero=FALSE)}
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
  \item{electrode_polarity}{either "pos" or "neg": how is the polarity of the Electrode: E = E0 -(RT/F)ln(H+) ("pos") or -E = E0 -(RT/F)ln(H+) ("neg")?}
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
  \item{verbose}{verbose mode: show the traject of the fitting in a
    plot}
  \item{k1k2}{either "roy" (default, Roy1993a) or "lueker" (Lueker2000, calculated with seacarb) for K\_CO2 and K\_HCO3.}
  \item{khf}{either "dickson" (default, Dickson1979a) or "perez"
    (Perez1987a, calculated with seacarb) for K\_HF}
  \item{datxbegin}{at what x value (amount of titrant added) does the
    supplied curve start? (i.e. is the complete curve supplied or just a
    part?)}
  \item{SumCO2Zero}{should SumCO2==0?}
}
\value{
  a list of up to five values ([TA] in mol/kg-solution, [SumCO2] in mol/kg-solution, E0 in V, K1 in mol/kg-solution and on free scale, sum of the squared residuals)
}
\examples{\dontrun{

####################################
# Calculating TA from titration data
####################################

#### 1.) proof of concept ##########
####################################
####################################

# generate "data":
S <- 35
t <- 15

SumCO2     <- 0.002000
TA         <- 0.002200
initial_ae <- aquaenv(S=S, t=t, SumCO2=SumCO2, TA=TA)

mass_sample  <- 0.01  # the mass of the sample solution in kg
mass_titrant <- 0.003 # the total mass of the added titrant solution
                      # in kg
conc_titrant <- 0.01  # the concentration of the titrant solution in
                      # mol/kg-soln
S_titrant    <- 0.5   # the salinity of the titrant solution (the
                      # salinity of a solution with a ionic strength
                      # of 0.01 according to:
                      # I = (19.924 S) / (1000 - 1.005 S)
steps        <- 20    # the amount of steps the mass of titrant is
                      # added in 
type         <- "HCl"

ae <- titration(initial_ae, mass_sample, mass_titrant, conc_titrant,
                S_titrant, steps, type)

plot(ae, ae$delta_mass_titrant, what="pH", newdevice=FALSE)

# the input data for the TA fitting routine: a table with the added
# mass of the titrant and the resulting free scale pH
titcurve <- cbind(ae$delta_mass_titrant, ae$pH)


# for the TA fitting procedure all total quantities except SumCO2
# (SumNH4, SumH2S, SumH3PO4, SumSiOH4, SumHNO3, SumHNO2, SumBOH3,
# SumH2SO4, SumHF) need to be known. However, the latter three
# can be calculated from salinity as it is done in this example.

fit1 <- TAfit(initial_ae, titcurve, conc_titrant, mass_sample,
              S_titrant)
fit1

# E (V) values as input variables: generate E values using
# E0=0.4 V and the nernst equation
tottitcurve <- convert(titcurve[,2], "pHscale", "free2sws", S=S,
                       t=t)
# (Nernst equation relates E to TOTAL [H+] (DOE1994, p.7,
# ch.4, sop.3), BUT, if fluoride is present, its SWS, so
# we use SWS!
Etitcurve   <- cbind(titcurve[,1], (0.4 - ((PhysChemConst$R/10)
               *initial_ae$T/PhysChemConst$F)
               *log(10^-tottitcurve)))  # Nernst equation

fit2 <- TAfit(initial_ae, Etitcurve, conc_titrant, mass_sample,
              S_titrant, Evals=TRUE, verbose=TRUE)
fit2
          

# k_co2 fitting: one K_CO2 (k_co2) for the whole titration curve
# is fitted, i.e. there is NO correction for K_CO2 changes due to
# changing S due to mixing with the titrant
fit3 <- TAfit(initial_ae, titcurve, conc_titrant, mass_sample,
              S_titrant, K_CO2fit=TRUE)
fit3

# assume the titrant has the same salinity as the sample
# (and is made up of natural seawater, i.e. containing SumBOH4,
# SumH2SO4 and SumHF as functions of S), then the "right" K_CO2
# should be fitted i.e we do NOT give the argument S_titrant
# and set the flag seawater_titrant to TRUE
ae       <- titration(initial_ae, mass_sample, mass_titrant,
                      conc_titrant, steps=steps, type=type,
                      seawater_titrant=TRUE)
titcurve <- cbind(ae$delta_mass_titrant, ae$pH)

fit4 <- TAfit(initial_ae, titcurve, conc_titrant, mass_sample,
              K_CO2fit=TRUE, seawater_titrant=TRUE)
fit4

# fitting of TA, SumCO2, K_CO2 and E0
Etitcurve <- cbind(titcurve[,1], (0.4 - ((PhysChemConst$R/10)
                   *initial_ae$T/PhysChemConst$F)
                   *log(10^-titcurve[,2])))
fit5 <- TAfit(initial_ae, Etitcurve, conc_titrant, mass_sample,
              K_CO2fit=TRUE, seawater_titrant=TRUE, Evals=TRUE)
fit5

# fitting of non equally spaced data:
neqsptitcurve <- rbind(titcurve[1:9,], titcurve[11:20,])
fit6 <- TAfit(initial_ae, neqsptitcurve, conc_titrant,
               mass_sample, seawater_titrant=TRUE,
               equalspaced=FALSE)
fit6

#add some "noise" on the generated data
noisetitcurve <- titcurve * rnorm(length(titcurve),
                 mean=1, sd=0.01) #one percent error possible
plot(ae, ae$delta_mass_titrant, what="pH", type="l", col="red",
     xlim=c(0,0.003), ylim=c(3,8.1), newdevice=FALSE)
par(new=TRUE)
plot(noisetitcurve[,1],noisetitcurve[,2], type="l",
     xlim=c(0,0.003), ylim=c(3,8.1))

fit7 <- TAfit(initial_ae, noisetitcurve, conc_titrant,
              mass_sample, seawater_titrant=TRUE)
fit7




# 2.) test with generated data from Dickson1981 #
#################################################
#################################################

conc_titrant = 0.3     # mol/kg-soln
mass_sample  = 0.2     # kg
S_titrant    = 14.835  # is aequivalent to the ionic strength
                       # of 0.3 mol/kg-soln 
  
SumBOH3  = 0.00042 # mol/kg-soln
SumH2SO4 = 0.02824 # mol/kg-soln
SumHF    = 0.00007 # mol/kg-soln

# convert mass of titrant from g to kg
sam <- cbind(sample_dickson1981[,1]/1000, sample_dickson1981[,2]) 

dicksonfit <- TAfit(aquaenv(t=25, S=35, SumBOH3=SumBOH3,
                    SumH2SO4=SumH2SO4, SumHF=SumHF), sam,
                    conc_titrant, mass_sample,
                    S_titrant=S_titrant, debug=TRUE)
dicksonfit
#TA     Dickson1981: 0.00245
#SumCO2 Dickson1981: 0.00220

# => not exactly the same! why?



# a.) does salinity correction (S_titrant) matter or not?
##########################################################

# without salinity correction
dicksontitration1 <- titration(aquaenv(t=25, S=35, SumCO2=0.00220,
                               SumBOH3=SumBOH3, SumH2SO4=SumH2SO4,
                               SumHF=SumHF, TA=0.00245),
                               mass_sample=mass_sample,
                               mass_titrant=0.0025,
                               conc_titrant=conc_titrant,
                               steps=50, type="HCl")

# with salinity correction
dicksontitration2 <- titration(aquaenv(t=25, S=35, SumCO2=0.00220,
                               SumBOH3=SumBOH3, SumH2SO4=SumH2SO4,
                               SumHF=SumHF, TA=0.00245),
                               mass_sample=mass_sample,
                               mass_titrant=0.0025,
                               conc_titrant=conc_titrant,
                               S_titrant=S_titrant,
                               steps=50, type="HCl")          

plot(dicksontitration1, xval=dicksontitration1$delta_mass_titrant,
     what="pH", xlim=c(0,0.0025), ylim=c(3,8.2), newdevice=FALSE,
     col="red") 
par(new=TRUE)
plot(dicksontitration2, xval=dicksontitration2$delta_mass_titrant,
     what="pH", xlim=c(0,0.0025), ylim=c(3,8.2), newdevice=FALSE,
     col="blue") 
par(new=TRUE)
plot(sam[,1], sam[,2], type="l", xlim=c(0,0.0025), ylim=c(3,8.2))

# => salinity correction makes NO difference, because the relation
# between total sample and added titrant is very large:
# salinity only drops from 35 to 34.75105

#BUT: there is an offset between the "Dickson" curve and our curve:
plot(dicksontitration2$pH - sam[,2])


# b.) does it get better if we fit K_CO2 as well?
#################################################
dicksonfit2 <- TAfit(aquaenv(t=25, S=35, SumBOH3=SumBOH3,
                             SumH2SO4=SumH2SO4, SumHF=SumHF), sam,
                             conc_titrant, mass_sample,
			     S_titrant=S_titrant, debug=TRUE,
			     K_CO2fit=TRUE)
dicksonfit2
#TA     Dickson1981: 0.00245
#SumCO2 Dickson1981: 0.00220

# => yes it does, but it is not perfect yet!


# c.) differing K values
#########################
# Dickson uses fixed K values that are slightly different than ours
dicksontitration3 <- titration(aquaenv(t=25, S=35, SumCO2=0.00220,
                               SumBOH3=SumBOH3, SumH2SO4=SumH2SO4,
                               SumHF=SumHF, TA=0.00245, k_w=4.32e-14,
			       k_co2=1e-6, k_hco3=8.20e-10,
			       k_boh3=1.78e-9, k_hso4=(1/1.23e1),
			       k_hf=(1/4.08e2)),
			       mass_sample=mass_sample,
			       mass_titrant=0.0025,
			       conc_titrant=conc_titrant,
			       steps=50, type="HCl",
			       S_titrant=S_titrant, k_w=4.32e-14,
			       k_co2=1e-6, k_hco3=8.20e-10,
			       k_boh3=1.78e-9, k_hso4=(1/1.23e1),
			       k_hf=(1/4.08e2))          
plot(dicksontitration3, xval=dicksontitration3$delta_mass_titrant,
     what="pH", xlim=c(0,0.0025), ylim=c(3,8.2), newdevice=FALSE,
     col="blue") 
par(new=TRUE)
plot(sam[,1], sam[,2], type="l", xlim=c(0,0.0025), ylim=c(3,8.2))

plot(dicksontitration3$pH - sam[,2])
# => no offset between the pH curves

# => exactly the same curves!


dicksonfit3 <- TAfit(aquaenv(t=25, S=35, SumBOH3=SumBOH3,
                     SumH2SO4=SumH2SO4, SumHF=SumHF, k_w=4.32e-14,
                     k_co2=1e-6, k_hco3=8.20e-10, k_boh3=1.78e-9,
		     k_hso4=(1/1.23e1), k_hf=(1/4.08e2)),
                     sam, conc_titrant, mass_sample,
		     S_titrant=S_titrant, debug=TRUE,
                     k_w=4.32e-14, k_co2=1e-6, k_hco3=8.20e-10,
		     k_boh3=1.78e-9, k_hso4=(1/1.23e1),
		     k_hf=(1/4.08e2))
dicksonfit3

# PERFECT fit!

plot(sam[,1], sam[,2], xlim=c(0,0.0025), ylim=c(3,8.2), type="l")
par(new=TRUE)
plot(tit$delta_mass_titrant, calc, xlim=c(0,0.0025), ylim=c(3,8.2),
     type="l", col="red")

}}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\keyword{misc}


