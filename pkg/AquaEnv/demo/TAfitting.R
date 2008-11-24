par(ask=TRUE)

##############################################################################################
# Calculating TA from titration data
##############################################################################################

#### 1.) proof of concept ###########################
#####################################################
#####################################################

# generate "data":
Tc <- 15
S  <- 35

SumCO2     <- 0.002000
TA         <- 0.002200
initial_ae <- aquaenv(Tc=Tc, S=S, SumCO2=SumCO2, TA=TA)

mass_sample  <- 0.01  # the mass of the sample solution in kg
mass_titrant <- 0.003 # the total mass of the added titrant solution in kg
conc_titrant <- 0.01  # the concentration of the titrant solution in mol/kg-soln
S_titrant    <- 0.5   # the salinity of the titrant solution (the salinity of a solution with a ionic strength of 0.01 according to: I = (19.924 S) / (1000 - 1.005 S)
steps        <- 20    # the amount of steps the mass of titrant is added in 
type         <- "HCl"

ae <- titration(initial_ae, mass_sample, mass_titrant, conc_titrant, S_titrant, steps, type)

plot(ae, ae$delta_mass_titrant, what="pH", newdevice=FALSE)

# the input data for the TA fitting routine: a table with the added mass of the titrant and the resulting free scale pH
titcurve <- cbind(ae$delta_mass_titrant, ae$pH)


# for the TA fitting procedure all total quantities except SumCO2 (SumNH4, SumH2S, SumH3PO4, SumSiOH4, SumHNO3, SumHNO2, SumBOH3, SumH2SO4, SumHF)
# need to be known. However, the latter three can be calculated from salinity as it is done in this example.

fit1 <- TAfit(initial_ae, titcurve, conc_titrant, mass_sample, S_titrant)
fit1

# E (V) values as input variables: generate E values using E0=0.4 V and the nernst equation
tottitcurve <- convert(titcurve[,2], "pHscale", "free2sws", Tc=Tc, S=S)  # Nernst equation relates E to TOTAL [H+] (DOE1994, p.7, ch.4, sop.3), BUT, if fluoride is present, its SWS, so we use SWS!
Etitcurve   <- cbind(titcurve[,1], (0.4 - ((Constants$R/10)*initial_ae$Tk/Constants$F)*log(10^-tottitcurve)))  # Nernst equation

fit2 <- TAfit(initial_ae, Etitcurve, conc_titrant, mass_sample, S_titrant, Evals=TRUE, verbose=TRUE)
fit2
          

# k_co2 fitting: one K_CO2 (k_co2) for the whole titration curve is fitted, i.e. there is NO correction for K_CO2 changes due to changing S due to mixing with the titrant
fit3 <- TAfit(initial_ae, titcurve, conc_titrant, mass_sample, S_titrant, K_CO2fit=TRUE)
fit3

# assume the titrant has the same salinity as the sample (and is made up of natural seawater, i.e. containing SumBOH4, SumH2SO4 and SumHF as functions of S), then the "right" K_CO2 should be fitted
# i.e we do NOT give the argument S_titrant and set the flag seawater_titrant to TRUE
ae       <- titration(initial_ae, mass_sample, mass_titrant, conc_titrant, steps=steps, type=type, seawater_titrant=TRUE)
titcurve <- cbind(ae$delta_mass_titrant, ae$pH)

fit4 <- TAfit(initial_ae, titcurve, conc_titrant, mass_sample, K_CO2fit=TRUE, seawater_titrant=TRUE)
fit4

# fitting of TA, SumCO2, K_CO2 and E0
Etitcurve <- cbind(titcurve[,1], (0.4 - ((Constants$R/10)*initial_ae$Tk/Constants$F)*log(10^-titcurve[,2])))
fit5 <- TAfit(initial_ae, Etitcurve, conc_titrant, mass_sample, K_CO2fit=TRUE, seawater_titrant=TRUE, Evals=TRUE)
fit5

# fitting of non equally spaced data:
neqsptitcurve <- rbind(titcurve[1:9,], titcurve[11:20,])
fit6 <- TAfit(initial_ae, neqsptitcurve, conc_titrant, mass_sample, seawater_titrant=TRUE, equalspaced=FALSE)
fit6

#add some "noise" on the generated data
noisetitcurve <- titcurve * rnorm(length(titcurve), mean=1, sd=0.01) #one percent error possible
plot(ae, ae$delta_mass_titrant, what="pH", type="l", col="red", xlim=c(0,0.003), ylim=c(3,8.1), newdevice=FALSE)
par(new=TRUE)
plot(noisetitcurve[,1],noisetitcurve[,2], type="l", xlim=c(0,0.003), ylim=c(3,8.1))

fit7 <- TAfit(initial_ae, noisetitcurve, conc_titrant, mass_sample, seawater_titrant=TRUE)
fit7





######## 2.) test with generated data from Dickson1981 ########
###############################################################
###############################################################

conc_titrant = 0.3     # mol/kg-soln
mass_sample  = 0.2     # kg
S_titrant    = 14.835  # is aequivalent to the ionic strength of 0.3 mol/kg-soln 
  
SumBOH3  = 0.00042 # mol/kg-soln
SumH2SO4 = 0.02824 # mol/kg-soln
SumHF    = 0.00007 # mol/kg-soln

sam <- cbind(sample_dickson1981[,1]/1000, sample_dickson1981[,2]) # convert mass of titrant from g to kg

dicksonfit <- TAfit(aquaenv(Tc=25, S=35, SumBOH3=SumBOH3, SumH2SO4=SumH2SO4, SumHF=SumHF), sam, conc_titrant, mass_sample, S_titrant=S_titrant, debug=TRUE)
dicksonfit
#TA     Dickson1981: 0.00245
#SumCO2 Dickson1981: 0.00220

# => not exactly the same! why?



# a.) does salinity correction (S_titrant) matter or not?
##########################################################

# without salinity correction
dicksontitration1 <- titration(aquaenv(Tc=25, S=35, SumCO2=0.00220, SumBOH3=SumBOH3, SumH2SO4=SumH2SO4, SumHF=SumHF, TA=0.00245),
                              mass_sample=mass_sample, mass_titrant=0.0025, conc_titrant=conc_titrant, steps=50, type="HCl")

# with salinity correction
dicksontitration2 <- titration(aquaenv(Tc=25, S=35, SumCO2=0.00220, SumBOH3=SumBOH3, SumH2SO4=SumH2SO4, SumHF=SumHF, TA=0.00245),
                              mass_sample=mass_sample, mass_titrant=0.0025, conc_titrant=conc_titrant, S_titrant=S_titrant, steps=50, type="HCl")          

plot(dicksontitration1, xval=dicksontitration1$delta_mass_titrant, what="pH", xlim=c(0,0.0025), ylim=c(3,8.2), newdevice=FALSE, col="red", newdevice=FALSE) 
par(new=TRUE)
plot(dicksontitration2, xval=dicksontitration2$delta_mass_titrant, what="pH", xlim=c(0,0.0025), ylim=c(3,8.2), newdevice=FALSE, col="blue", newdevice=FALSE) 
par(new=TRUE)
plot(sam[,1], sam[,2], type="l", xlim=c(0,0.0025), ylim=c(3,8.2))

# => salinity correction makes NO difference, because the relation between total sample and added titrant is very large: salinity only drops from 35 to 34.75105

#BUT: there is an offset between the "Dickson" curve and our curve:
plot(Dicksontitration2$pH - sam[,2])


# b.) does it get better if we fit K_CO2 as well?
#################################################
dicksonfit2 <- TAfit(aquaenv(Tc=25, S=35, SumBOH3=SumBOH3, SumH2SO4=SumH2SO4, SumHF=SumHF), sam, conc_titrant, mass_sample, S_titrant=S_titrant, debug=TRUE, K_CO2fit=TRUE)
dicksonfit2
#TA     Dickson1981: 0.00245
#SumCO2 Dickson1981: 0.00220

# => yes it does, but it is not perfect yet!


# c.) differing K values
#########################
# Dickson uses fixed K values that are slightly different than ours
dicksontitration3 <- titration(aquaenv(Tc=25, S=35, SumCO2=0.00220, SumBOH3=SumBOH3, SumH2SO4=SumH2SO4, SumHF=SumHF, TA=0.00245, k_w=4.32e-14, k_co2=1e-6, k_hco3=8.20e-10, k_boh3=1.78e-9, k_hso4=(1/1.23e1), k_hf=(1/4.08e2)),
                              mass_sample=mass_sample, mass_titrant=0.0025, conc_titrant=conc_titrant,  steps=50, type="HCl", S_titrant=S_titrant,
                              k_w=4.32e-14, k_co2=1e-6, k_hco3=8.20e-10, k_boh3=1.78e-9, k_hso4=(1/1.23e1), k_hf=(1/4.08e2))          

plot(dicksontitration3, xval=dicksontitration3$delta_mass_titrant, what="pH", xlim=c(0,0.0025), ylim=c(3,8.2), newdevice=FALSE, col="blue", newdevice=FALSE) 
par(new=TRUE)
plot(sam[,1], sam[,2], type="l", xlim=c(0,0.0025), ylim=c(3,8.2))

plot(dicksontitration3$pH - sam[,2])
# => no offset between the pH curves

# => exactly the same curves!


dicksonfit3 <- TAfit(aquaenv(Tc=25, S=35, SumBOH3=SumBOH3, SumH2SO4=SumH2SO4, SumHF=SumHF, k_w=4.32e-14, k_co2=1e-6, k_hco3=8.20e-10, k_boh3=1.78e-9, k_hso4=(1/1.23e1), k_hf=(1/4.08e2)),
                     sam, conc_titrant, mass_sample, S_titrant=S_titrant, debug=TRUE,
                     k_w=4.32e-14, k_co2=1e-6, k_hco3=8.20e-10, k_boh3=1.78e-9, k_hso4=(1/1.23e1), k_hf=(1/4.08e2))
dicksonfit3

# PERFECT fit!

plot(sam[,1], sam[,2], xlim=c(0,0.0025), ylim=c(3,8.2), type="l")
par(new=TRUE)
plot(tit$delta_mass_titrant, calc, xlim=c(0,0.0025), ylim=c(3,8.2), type="l", col="red")






#### 2.) real data ###########################
##############################################
##############################################

# sample1 is a titration curve for TA determination, obtained in the lab of the  NIOO-CEME, Yerseke, The Netherlands
####################################################################################################################

# Metadata:
# x-value = volume of titrant added (in ml) with a Metrohm Omega 665 Dosimat
# y-value = pH measured on the NBS scale with a Metrohm Omega 714 pH Meter (Buffer pH 4: citric acid, sodium hydroxide, chlor hydrogen; Buffer pH 7: di-sodium hydrogen phosphate, potassium di-hydrogen phosphate
# Tc = 23.5 degC
# S  = 34
# pH = 8.71 (at the start of the titration)
# volume of the sample = 10 ml
# titrant type = HCl
# titrant conc = 0.01 N = 0.01 M


# assume the salinity of the titrant solution is 0.5 (the salinity of a solution with a ionic strength of 0.01 mol/kg-solution (~0.01N) according to: I = (19.924 S) / (1000 - 1.005 S)
# convert the x values to kg titrant, assume the density of the titrant solution is aequivalent to the density of seawater with S=0.5
# 1 ml = 0.000001 m3
sample       <- cbind((sample1[,1] * 0.000001 * aquaenv(Tc=23.5, S=0.5)$density), sample1[,2])

# reduce the amount of datapoints: for runtime reasons
sample       <- sample[seq(1,length(sample[,1]), 3),]

# convert the concentration of the titrant from molarity (mol/l) to mol/kg-soln
conc_titrant <- convert(0.01, "conc", "molar2molin", Tc=23.5, S=0.5)

# calculate the mass of the sample in kg (10 ml = 0.00001 m3)
mass_sample  <- 0.00001 * aquaenv(Tc=23.5, S=34)$density

# perform the fitting procedure: note that the datapoints are NOT equally spaced and the pH scale of the measured pH values is the nbs scale,
# debug=TRUE toggles the debug mode: the last simulated titration tit and the converted pH profile calc are made global variables for investigation and plotting
sample1_fit <- TAfit(aquaenv(Tc=23.5, S=34), sample, conc_titrant, mass_sample, S_titrant=0.5, equalspaced=FALSE, pHscale="nbs", debug=TRUE)
sample1_fit

xlim <- range(sample[,1], tit$delta_mass_titrant)
ylim <- range(sample[,2], calc)
plot(sample[,1], sample[,2], xlim=xlim, ylim=ylim, type="l")
par(new=TRUE)
plot(tit$delta_mass_titrant, calc, xlim=xlim, ylim=ylim, type="l", col="red")

