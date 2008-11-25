\name{aquaenv}
\alias{aquaenv}
\title{aquaenv}
\description{PUBLIC function: the main function of the package AquaEnv: creates an object of class aquaenv}
\usage{aquaenv(Tc, S, d=0, SumCO2=0, SumNH4=0, SumH2S=0, SumH3PO4=0,
        SumSiOH4=0, SumHNO3=0, SumHNO2=0, SumBOH3=NULL,
        SumH2SO4=NULL,SumHF=NULL, TA=NULL, pH=NULL, pCO2=NULL, CO2=NULL,
        speciation=TRUE, dsa=FALSE, ae=NULL, from.data.frame=FALSE,
        SumH2SO4_Koffset=0,SumHF_Koffset=0, revelle=FALSE, skeleton=FALSE,
        k_w=NULL, k_co2=NULL, k_hco3=NULL, k_boh3=NULL, k_hso4=NULL,
        k_hf=NULL, k1k2="roy", khf="dickson")}
\arguments{
  \item{Tc }{temperature in degrees centigrade}
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{d }{depth in meters}
  \item{SumCO2 }{total carbonate concentration in mol/kg-solution, if NULL is supplied it is calculated}
  \item{SumNH4 }{total ammonium concentration in mol/kg-solution, optional}
  \item{SumH2S }{total sulfide concentration in mol/kg-solution, optional}
  \item{SumH3PO4 }{total phosphate concentration in mol/kg-solution, optional}
  \item{SumSiOH4 }{total silicate concentration in mol/kg-solution, optional}
  \item{SumHNO3 }{total nitrate concentration in mol/kg-solution, optional}
  \item{SumHNO2 }{total nitrite concentration in mol/kg-solution, optional}
  \item{SumBOH3 }{total borate concentration in mol/kg-solution, calculated from S if not supplied}
  \item{SumH2SO4 }{total sulfate concentration in mol/kg-solution, calculated from S if not supplied}
  \item{SumHF }{total fluoride concentration in mol/kg-solution, calculated from S if not supplied}
  \item{TA }{total alkalinity in mol/kg-solution, if supplied, pH will be calculated}
  \item{pH }{pH on the free proton concentration scale, if supplied, total alkalinity will be calculated}
  \item{pCO2 }{partial pressure of CO2 in atm (note that we do not destinguish between partial pressure and fugacity), can be used with either [TA], pH, or [CO2] to define the system}
  \item{CO2 }{concentration of CO2, can be used with either [TA], pH, or pCO2 to define the system }
  \item{speciation }{flag: TRUE = full speciation is calculated}
  \item{dsa }{flag: TRUE = all information necessary to build a pH model with the direct substitution approach (DSA, Hofmann2008) is calculated}
  \item{ae }{either an object of class aquaenv used for the cloning functionality or a dataframe used for the from.data.frame
    functionality. Note that for cloning the desired k1k2 and khf values need to be specified! (otherwise the default values are used for the cloned object)}
  \item{from.data.frame }{flag: TRUE = the object of class aquaenv is built from the data frame supplied in ae}
  \item{SumH2SO4_Koffset }{only used internally to calculate dTAdKdKdSumH2SO4}
  \item{SumHF_Koffset }{only used internally to calculate dTAdKdKdSumHF}
  \item{revelle }{flag: TRUE = the revelle factor is calculated}
  \item{skeleton }{flag: TRUE = a reduced amount of information is calculated yielding a smaller object of type aquaenv}
  \item{k_w}{a fixed K\_W can be specified}
  \item{k_co2}{a fixed K\_CO2 can be specified; used for TA fitting: give a K\_CO2 and NOT calculate it from T and S: i.e. K\_CO2 can be fitted in the routine as well}
  \item{k_hco3}{a fixed K\_HCO3 can be specified}
  \item{k_boh3}{a fixed K\_BOH3 can be specified}
  \item{k_hso4}{a fixed K\_HSO4 can be specified}
  \item{k_hf}{a fixed K\_HF can be specified}
  \item{k1k2}{either "roy" (default, Roy1993a) or "lueker" (Lueker2000, calculated with seacarb) for K\_CO2 and K\_HCO3.}
  \item{khf}{either "dickson" (default, Dickson1979a) or "perez" (Perez1987a, calculated with seacarb) for K\_HF}
}
\examples{\dontrun{
############################
# Minimal aquaenv definition
############################
ae <- aquaenv(Tc=15, S=30)
ae$K_CO2

ae$Ksp_calcite
ae$Ksp_aragonite


ae <- aquaenv(Tc=15, S=30, d=10)
ae$K_CO2

ae$Ksp_calcite
ae$Ksp_aragonite

ae


########################################################
# Defining the complete aquaenv system in different ways
########################################################
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



################################################################
# Cloning the aquaenv system: 1 to 1 and with different pH or TA
################################################################
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


##################################################################
# Calculating SumCO2 by giving a constant  pH&CO2, pH&pCO2, pH&TA,
# TA&CO2, or TA&pCO2
##################################################################
pCO2   <- 0.0006952296
CO2    <- 2.678134e-05
pH     <- 7.888569
TA     <- 0.0021

Tc <- 15
S  <- 30
d  <- 10

ae <- aquaenv(Tc, S, d, SumCO2=NULL, pH=pH, CO2=CO2, dsa=TRUE,
              revelle=TRUE)

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

ae <- aquaenv(Tc, S, d, SumCO2=NULL, pH=pH, CO2=CO2, revelle=TRUE,
              dsa=TRUE)
plot(ae, xval=Tc, xlab="T/(deg C)", newdevice=FALSE)

Tc     <- 15
S      <- 1:30
ae <- aquaenv(Tc, S, d, SumCO2=NULL, pH=pH, pCO2=pCO2, revelle=TRUE,
              dsa=TRUE)
plot(ae, xval=S, xlab="S", newdevice=FALSE)

S <- 30
d <- seq(1,1000, 100)
ae <- aquaenv(Tc, S, d, SumCO2=NULL, pH=pH, TA=TA, revelle=TRUE,
              dsa=TRUE)
plot(ae, xval=d, xlab="depth/m", newdevice=FALSE)

}}
\value{
  a list containing: 
	      "Tc"               "Tk"               "S"               
	      "Cl"               "I"                "d"               
	      "hydroP"           "density"          "SumCO2"          
	      "SumNH4"           "SumH2S"           "SumHNO3"         
	      "SumHNO2"          "SumH3PO4"         "SumSiOH4"        
   	      "SumBOH3"          "SumH2SO4"         "SumHF"           
              "SumBr"            "ClConc"           "Na"              
              "Mg"               "Ca"               "K"               
              "Sr"               "molal2molin"      "free2tot"        
              "free2sws"         "tot2free"         "tot2sws"         
              "sws2free"         "sws2tot"          "K0\_CO2"          
              "K0\_O2"            "CO2\_sat"          "O2\_sat"          
              "K\_W"              "K\_HSO4"           "K\_HF"            
              "K\_CO2"            "K\_HCO3"           "K\_BOH3"          
              "K\_NH4"            "K\_H2S"            "K\_H3PO4"         
              "K\_H2PO4"          "K\_HPO4"           "K\_SiOH4"         
              "K\_SiOOH3"         "K\_HNO2"           "K\_HNO3"          
              "K\_H2SO4"          "K\_HS"             "Ksp\_calcite"     
              "Ksp\_aragonite"    "TA"               "pH"              
              "pCO2"             "CO2"              "HCO3"            
              "CO3"              "BOH3"             "BOH4"            
              "OH"               "H3PO4"            "H2PO4"           
              "HPO4"             "PO4"              "SiOH4"           
              "SiOOH3"           "SiO2OH2"          "H2S"             
              "HS"               "S2min"            "NH4"             
              "NH3"              "H2SO4"            "HSO4"            
              "SO4"              "HF"               "F"               
              "HNO3"             "NO3"              "HNO2"            
              "NO2"              "omega\_calcite"    "omega\_aragonite" 
              "revelle"          "c1"               "c2"              
              "c3"               "dTAdSumCO2"       "b1"              
              "b2"               "dTAdSumBOH3"      "so1"             
              "so2"              "so3"              "dTAdSumH2SO4"    
              "f1"               "f2"               "dTAdSumHF"       
              "dTAdH"            "dTAdKdKdS"        "dTAdKdKdT"       
              "dTAdKdKdd"        "dTAdKdKdSumH2SO4" "dTAdKdKdSumHF"   
		or a subset of this set. Please consult the vignette of AquaEnv for more details
	      }
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\keyword{misc}

