\name{aquaenv}
\alias{aquaenv}
\title{aquaenv}
\description{PUBLIC function: the main function of the package AquaEnv: creates an object of class aquaenv}
\usage{aquaenv(S, t, p=pmax((P-Pa), gauge_p(d, lat, Pa)),
        P=Pa, Pa=1.01325, d=0, lat=0,
        SumCO2=0, SumNH4=0, SumH2S=0, SumH3PO4=0,
        SumSiOH4=0, SumHNO3=0, SumHNO2=0, SumBOH3=NULL,
        SumH2SO4=NULL, SumHF=NULL, TA=NULL, pH=NULL, fCO2=NULL, CO2=NULL,
        speciation=TRUE, dsa=FALSE, ae=NULL, from.data.frame=FALSE,
        SumH2SO4_Koffset=0, SumHF_Koffset=0, revelle = TRUE, skeleton=FALSE,
        k_w=NULL, k_co2=NULL, k_hco3=NULL, k_boh3=NULL, k_hso4=NULL,
        k_hf=NULL, k1k2="lueker", khf="dickson", khso4="dickson", fCO2atm=0.000400, fO2atm=0.20946)}
\arguments{
  \item{S }{salinity in practical salinity units (i.e. no unit)}
  \item{t }{temperature in degrees centigrade}
  \item{p }{gauge pressure (total pressure minus atmospheric pressure)
            in bars, standard is calculated either from the given P,
            or the given d, lat, and Pa}
  \item{P }{total pressure in bars, standard: Pa (at the surface)}
  \item{Pa }{atmospheric pressure in bars, standard: 1 atm (at sea-level) }
  \item{d }{depth below the surface in meters, standard: 0 (at the surface)}
  \item{lat }{latitude in degrees (-90 to +90) to calculate the
    gravitational constant g for calculating the water depth from the
    pressure and vice versa, standard: 0}
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
  \item{fCO2 }{fugacity of CO2 in the water in atm (i.e. the fugacity of
    CO2 in a small volume of air fully equilibrated with a sufficiently
    large sample of water), can be used with either [TA], pH, or [CO2] to define the system}
  \item{CO2 }{concentration of CO2, can be used with either [TA], pH, or fCO2 to define the system }
  \item{speciation }{flag: TRUE = full speciation is calculated}
  \item{dsa }{flag: TRUE = all information necessary to build a pH model with the direct substitution approach (DSA, Hofmann2008) is calculated}
  \item{ae }{either an object of class aquaenv used for the cloning functionality or a dataframe used for the from.data.frame
    functionality. Note that for cloning the desired k1k2 and khf values need to be specified! (otherwise the default values are used for the cloned object)}
  \item{from.data.frame }{flag: TRUE = the object of class aquaenv is built from the data frame supplied in ae}
  \item{SumH2SO4_Koffset }{only used internally to calculate dTAdKdKdSumH2SO4}
  \item{SumHF_Koffset }{only used internally to calculate dTAdKdKdSumHF}
  \item{revelle }{redundant argument - present for backward compatibility}
  \item{skeleton }{flag: TRUE = a reduced amount of information is calculated yielding a smaller object of type aquaenv}
  \item{k_w}{a fixed K\_W can be specified}
  \item{k_co2}{a fixed K\_CO2 can be specified; used for TA fitting: give a K\_CO2 and NOT calculate it from T and S: i.e. K\_CO2 can be fitted in the routine as well}
  \item{k_hco3}{a fixed K\_HCO3 can be specified}
  \item{k_boh3}{a fixed K\_BOH3 can be specified}
  \item{k_hso4}{a fixed K\_HSO4 can be specified}
  \item{k_hf}{a fixed K\_HF can be specified}
  \item{k1k2}{either "lueker" (default, Lueker2000), "roy" (Roy1993a), 
    or "millero" (Millero2006) for K\_CO2 and K\_HCO3.}
  \item{khf}{either "dickson" (default, Dickson1979a) or "perez"
    (Perez1987a) for K\_HF}
  \item{khso4}{either "dickson" (default, Dickson1990) or "khoo" (Khoo1977) for
    K\_HSO4}
  \item{fCO2atm}{atmospheric fugacity of CO2 in atm, default =
    0.000400 atm}
  \item{fO2atm}{atmospheric fugacity of O2 in atm, default =
    0.20946 atm}
}
\examples{\dontrun{
############################
# Minimal aquaenv definition
############################
ae <- aquaenv(S=30, t=15)
ae$K_CO2

ae$Ksp_calcite
ae$Ksp_aragonite

ae <- aquaenv(S=30, t=15, p=10)
ae <- aquaenv(S=30, t=15, P=11)
ae <- aquaenv(S=30, t=15, d=100)
ae <- aquaenv(S=30, t=15, d=100, Pa=0.5)
ae$K_CO2

ae$Ksp_calcite
ae$Ksp_aragonite

ae


########################################################
# Defining the complete aquaenv system in different ways
########################################################
S      <- 30
t      <- 15
p      <- gauge_p(d=10)  # ~ p <- 0.1*10*1.01325
SumCO2 <- 0.0020
pH     <- 8
TA     <- 0.002140798
fCO2   <- 0.0005326744
CO2    <- 2.051946e-05

ae <- aquaenv(S, t, p, SumCO2=SumCO2, pH=pH)
ae$TA

ae <- aquaenv(S, t, p, SumCO2=SumCO2, TA=TA)
ae$pH

ae <- aquaenv(S, t, p, SumCO2=SumCO2, CO2=CO2)
ae$pH

ae <- aquaenv(S, t, p, SumCO2=SumCO2, fCO2=fCO2)
ae$pH

ae <- aquaenv(S, t, p, SumCO2=SumCO2, CO2=CO2, fCO2=fCO2)
ae <- aquaenv(S, t, p, SumCO2=SumCO2, pH=pH, TA=TA)
ae <- aquaenv(S, t, p, SumCO2=SumCO2, pH=pH, CO2=CO2)
ae <- aquaenv(S, t, p, SumCO2=SumCO2, pH=pH, fCO2=fCO2)
ae <- aquaenv(S, t, p, SumCO2=SumCO2, TA=TA, CO2=CO2)
ae <- aquaenv(S, t, p, SumCO2=SumCO2, TA=TA, fCO2=fCO2)


################################################################
# Cloning the aquaenv system: 1 to 1 and with different pH or TA
################################################################
S      <- 30
t      <- 15
SumCO2 <- 0.0020
TA     <- 0.00214


ae <- aquaenv(S, t, SumCO2=SumCO2, TA=TA)

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

S      <- 30
t      <- 1:15
p      <- gauge_p(10)
ae <- aquaenv(S, t, p, SumCO2=SumCO2, pH=pH, revelle=TRUE, dsa=TRUE)
plot(ae, xval=t, xlab="T/(deg C)", newdevice=FALSE)

S  <- 1:30
t  <- 15
ae <- aquaenv(S, t, p, SumCO2=SumCO2, pH=pH, revelle=TRUE, dsa=TRUE)
plot(ae, xval=S, xlab="S", newdevice=FALSE)

S  <- 30
p  <- gauge_p(seq(1,1000, 100))
ae <- aquaenv(S, t, p, SumCO2=SumCO2, pH=pH, revelle=TRUE, dsa=TRUE)
plot(ae, xval=p, xlab="gauge pressure/bar", newdevice=FALSE)



TA <- 0.0023

S  <- 30
t  <- 1:15
d  <- gauge_p(10)
ae <- aquaenv(S, t, p, SumCO2=SumCO2, TA=TA, revelle=TRUE, dsa=TRUE)
plot(ae, xval=t, xlab="T/(deg C)", newdevice=FALSE)

S  <- 1:30
t  <- 15
ae <- aquaenv(S, t, p, SumCO2=SumCO2, TA=TA, revelle=TRUE, dsa=TRUE)
plot(ae, xval=S, xlab="S", newdevice=FALSE)

S  <- 30
p  <- gauge_p(seq(1,1000, 100))
ae <- aquaenv(S, t, p, SumCO2=SumCO2, TA=TA, revelle=TRUE, dsa=TRUE)
plot(ae, xval=p, xlab="gauge pressure/bar", newdevice=FALSE)



##################################################################
# Calculating SumCO2 by giving a constant  pH&CO2, pH&fCO2, pH&TA,
# TA&CO2, or TA&fCO2
##################################################################
fCO2   <- 0.0006952296
CO2    <- 2.678137e-05
pH     <- 7.888573
TA     <- 0.0021

S  <- 30
t  <- 15
p  <- gauge_p(10)

ae <- aquaenv(S, t, p, SumCO2=NULL, pH=pH, CO2=CO2, dsa=TRUE, revelle=TRUE)
ae$SumCO2
ae$revelle
ae$dTAdH

ae <- aquaenv(S, t, p, SumCO2=NULL, pH=pH, fCO2=fCO2)
ae$SumCO2

ae <- aquaenv(S, t, p, SumCO2=NULL, pH=pH, TA=TA)
ae$SumCO2

ae <- aquaenv(S, t, p, SumCO2=NULL, TA=TA, CO2=CO2)
ae$SumCO2

ae <- aquaenv(S, t, p, SumCO2=NULL, TA=TA, fCO2=fCO2)
ae$SumCO2


t  <- 1:15
ae <- aquaenv(S, t, p, SumCO2=NULL, pH=pH, CO2=CO2)
plot(ae, xval=t, xlab="T/(deg C)", mfrow=c(9,10), newdevice=FALSE)

ae <- aquaenv(S, t, p, SumCO2=NULL, pH=pH, CO2=CO2, revelle=TRUE, dsa=TRUE)
plot(ae, xval=t, xlab="T/(deg C)", newdevice=FALSE)

S  <- 1:30
t  <- 15
ae <- aquaenv(S, t, p, SumCO2=NULL, pH=pH, fCO2=fCO2, revelle=TRUE, dsa=TRUE)
plot(ae, xval=S, xlab="S", newdevice=FALSE)

S  <- 30
p  <- gauge_p(seq(1,1000, 100))
ae <- aquaenv(S, t, p, SumCO2=NULL, pH=pH, TA=TA, revelle=TRUE, dsa=TRUE)
plot(ae, xval=p, xlab="gauge pressure/bar", newdevice=FALSE)

}}
\value{
  a list containing: 
              "S"                "t"                "p"
              "T"                "Cl"               "I"
              "P"                "Pa"               "d"
	      "density"          "SumCO2"          
	      "SumNH4"           "SumH2S"           "SumHNO3"         
	      "SumHNO2"          "SumH3PO4"         "SumSiOH4"        
   	      "SumBOH3"          "SumH2SO4"         "SumHF"           
              "Br"               "ClConc"           "Na"              
              "Mg"               "Ca"               "K"               
              "Sr"               "molal2molin"      "free2tot"        
              "free2sws"         "tot2free"         "tot2sws"         
              "sws2free"         "sws2tot"          "K0\_CO2"          
              "K0\_O2"           "fCO2atm"          "fO2atm"
	      "CO2\_sat"          "O2\_sat"
              "K\_W"              "K\_HSO4"           "K\_HF"            
              "K\_CO2"            "K\_HCO3"           "K\_BOH3"          
              "K\_NH4"            "K\_H2S"            "K\_H3PO4"         
              "K\_H2PO4"          "K\_HPO4"           "K\_SiOH4"         
              "K\_SiOOH3"         "K\_HNO2"           "K\_HNO3"          
              "K\_H2SO4"          "K\_HS"             "Ksp\_calcite"     
              "Ksp\_aragonite"    "TA"               "pH"              
              "fCO2"             "CO2"              "HCO3"            
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
\author{Andreas F. Hofmann. Maintained by Karline Soetaert (Karline.Soetaert@nioz.nl).}
\keyword{misc}

