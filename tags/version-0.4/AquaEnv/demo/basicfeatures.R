par(ask=TRUE)

#######################################################################
# calling the K functions directly
#######################################################################
K1 <- K_CO2(15,30)
K2 <- K_HCO3(15,30)

K1
K2



#######################################################################
# Minimal aquaenv definition
#######################################################################
ae <- aquaenv(Tc=15, S=30)
ae$K_CO2

ae$Ksp_calcite
ae$Ksp_aragonite


ae <- aquaenv(Tc=15, S=30, d=10)
ae$K_CO2

ae$Ksp_calcite
ae$Ksp_aragonite

ae


#######################################################################
# Defining the complete aquaenv system in different ways
#######################################################################
Tc     <- 15
S      <- 30
d      <- 10
SumCO2 <- 0.0020
pH     <- 8
TA     <- 0.002140323
pCO2   <- 0.000533576
CO2    <- 2.055419e-05

ae <- aquaenv(Tc, S, d, SumCO2=SumCO2, pH=pH)
ae$TA

ae <- aquaenv(Tc, S, d, SumCO2=SumCO2, TA=TA)
ae$pH

ae <- aquaenv(Tc, S, d, SumCO2=SumCO2, CO2=CO2)
ae$pH

ae <- aquaenv(Tc, S, d, SumCO2=SumCO2, pCO2=pCO2)
ae$pH

ae <- aquaenv(Tc, S, d, SumCO2=SumCO2, CO2=CO2, pCO2=pCO2)
ae <- aquaenv(Tc, S, d, SumCO2=SumCO2, pH=pH, TA=TA)
ae <- aquaenv(Tc, S, d, SumCO2=SumCO2, pH=pH, CO2=CO2)
ae <- aquaenv(Tc, S, d, SumCO2=SumCO2, pH=pH, pCO2=pCO2)
ae <- aquaenv(Tc, S, d, SumCO2=SumCO2, TA=TA, CO2=CO2)
ae <- aquaenv(Tc, S, d, SumCO2=SumCO2, TA=TA, pCO2=pCO2)


###########################################################################################
# having "aquaenv" calculated all properties that are needed to use the DSA for pH modelling
############################################################################################
Tc     <- 15
S      <- 30
d      <- 10
SumCO2 <- 0.0020
pH     <- 8


ae <- aquaenv(Tc=Tc, S=S, d=d, SumCO2=SumCO2, pH=pH, dsa=TRUE, revelle=TRUE)

#the buffer factor and auxilary buffer factors
ae$dTAdH
ae$dTAdSumCO2
ae$dTAdSumBOH3
ae$dTAdSumH2SO4
ae$dTAdSumHF

#the buffer factors concerning changes in the K's
ae$dTAdKdKdS
ae$dTAdKdKdT
ae$dTAdKdKdd
ae$dTAdKdKdSumH2SO4
ae$dTAdKdKdSumHF

#the partitioning coefficients
ae$c1
ae$c2
ae$c3
ae$b1
ae$b2
ae$so1
ae$so2
ae$so3
ae$f1
ae$f2

#not really DSA relevant: the revelle factor
ae$revelle





#######################################################################
# Cloning the aquaenv system: 1 to 1 and with different pH or TA
#######################################################################
Tc     <- 15
S      <- 30
SumCO2 <- 0.0020
TA     <- 0.00214


ae <- aquaenv(Tc, S, SumCO2=SumCO2, TA=TA)

aeclone1 <- aquaenv(ae=ae)

pH <- 9

aeclone2 <- aquaenv(ae=ae, pH=pH)

TA <- 0.002

aeclone3 <- aquaenv(ae=ae, TA=TA)

ae$pH
aeclone1$pH
aeclone2$TA
aeclone3$pH



#######################################################################
#preparing input variables
#######################################################################
Tc <- 15
S  <- 10

pH_NBS      <- 8.142777
SumCO2molar <- 0.002016803

pH_free     <- convert(pH_NBS,      "pHscale", "nbs2free",    Tc=Tc, S=S)
SumCO2molin <- convert(SumCO2molar, "conc",    "molar2molin", Tc=Tc, S=S)

ae <- aquaenv(Tc, S, SumCO2=SumCO2molin, pH=pH_free)
ae$pH
ae$SumCO2
ae$TA



#########################################################################
# Vectors as input variables (only ONE input variable may be a vector)
# (with full output: including the Revelle factor and the DSA properties)
#########################################################################
SumCO2 <- 0.0020
pH     <- 8

Tc     <- 1:15
S      <- 30
d      <- 10
ae <- aquaenv(Tc, S, d, SumCO2=SumCO2, pH=pH, revelle=TRUE, dsa=TRUE)
plot(ae, xval=Tc, xlab="T/(deg C)", newdevice=FALSE)

Tc <- 15
S  <- 1:30
ae <- aquaenv(Tc, S, d, SumCO2=SumCO2, pH=pH, revelle=TRUE, dsa=TRUE)
plot(ae, xval=S, xlab="S", newdevice=FALSE)

S <- 30
d <- seq(1,1000, 100)
ae <- aquaenv(Tc, S, d, SumCO2=SumCO2, pH=pH, revelle=TRUE, dsa=TRUE)
plot(ae, xval=d, xlab="depth/m", newdevice=FALSE)



TA     <- 0.0023

Tc     <- 1:15
S      <- 30
d      <- 10
ae <- aquaenv(Tc, S, d, SumCO2=SumCO2, TA=TA, revelle=TRUE, dsa=TRUE)
plot(ae, xval=Tc, xlab="T/(deg C)", newdevice=FALSE)

Tc <- 15
S  <- 1:30
ae <- aquaenv(Tc, S, d, SumCO2=SumCO2, TA=TA, revelle=TRUE, dsa=TRUE)
plot(ae, xval=S, xlab="S", newdevice=FALSE)

S <- 30
d <- seq(1,1000, 100)
ae <- aquaenv(Tc, S, d, SumCO2=SumCO2, TA=TA, revelle=TRUE, dsa=TRUE)
plot(ae, xval=d, xlab="depth/m", newdevice=FALSE)


######################################################################################
# Conversion from and to a dataframe
#(conversion from a dataframe is needed to treat the results of a dynamic run with the
# package deSolve as an object of type "aquaenv")
######################################################################################
aedataframe <- as.data.frame(ae)
aetest      <- aquaenv(ae=aedataframe, from.data.frame=TRUE)



######################################################################################
# Calculating SumCO2 by giving a constant  pH&CO2, pH&pCO2, pH&TA, TA&CO2, or TA&pCO2  
######################################################################################
pCO2   <- 0.0006952296
CO2    <- 2.678134e-05
pH     <- 7.888569
TA     <- 0.0021

Tc <- 15
S  <- 30
d  <- 10

ae <- aquaenv(Tc, S, d, SumCO2=NULL, pH=pH, CO2=CO2, dsa=TRUE, revelle=TRUE)
ae$SumCO2
ae$revelle
ae$dTAdH

ae <- aquaenv(Tc, S, d, SumCO2=NULL, pH=pH, pCO2=pCO2)
ae$SumCO2

ae <- aquaenv(Tc, S, d, SumCO2=NULL, pH=pH, TA=TA)
ae$SumCO2

ae <- aquaenv(Tc, S, d, SumCO2=NULL, TA=TA, CO2=CO2)
ae$SumCO2

ae <- aquaenv(Tc, S, d, SumCO2=NULL, TA=TA, pCO2=pCO2)
ae$SumCO2


Tc     <- 1:15
ae <- aquaenv(Tc, S, d, SumCO2=NULL, pH=pH, CO2=CO2)
plot(ae, xval=Tc, xlab="T/(deg C)", mfrow=c(9,10), newdevice=FALSE)

ae <- aquaenv(Tc, S, d, SumCO2=NULL, pH=pH, CO2=CO2, revelle=TRUE, dsa=TRUE)
plot(ae, xval=Tc, xlab="T/(deg C)", newdevice=FALSE)

Tc     <- 15
S      <- 1:30
ae <- aquaenv(Tc, S, d, SumCO2=NULL, pH=pH, pCO2=pCO2, revelle=TRUE, dsa=TRUE)
plot(ae, xval=S, xlab="S", newdevice=FALSE)

S <- 30
d <- seq(1,1000, 100)
ae <- aquaenv(Tc, S, d, SumCO2=NULL, pH=pH, TA=TA, revelle=TRUE, dsa=TRUE)
plot(ae, xval=d, xlab="depth/m", newdevice=FALSE)





