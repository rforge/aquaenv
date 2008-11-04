\name{aquaenv}
\alias{aquaenv}
\title{aquaenv}
\description{PUBLIC function: the main function of the package AquaEnv: creates an object of class aquaenv}
\usage{aquaenv(Tc, S, d=0, SumCO2=0, SumNH4=0, SumH2S=0, SumH3PO4=0, SumSiOH4=0, SumHNO3=0, SumHNO2=0, SumBOH3=NULL, SumH2SO4=NULL,SumHF=NULL, TA=NULL, pH=NULL, pCO2=NULL, CO2=NULL, speciation=TRUE, dsa=FALSE, ae=NULL, from.data.frame=FALSE, SumH2SO4_Koffset=0,SumHF_Koffset=0, revelle=FALSE, skeleton=FALSE, K1=NULL)}
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
  \item{ae }{either an object of class aquaenv used for the cloning functionality or a dataframe used for the from.data.frame functionality}
  \item{from.data.frame }{flag: TRUE = the object of class aquaenv is built from the data frame supplied in ae}
  \item{SumH2SO4_Koffset }{only used internally to calculate dTAdKdKdSumH2SO4}
  \item{SumHF_Koffset }{only used internally to calculate dTAdKdKdSumHF}
  \item{revelle }{flag: TRUE = the revelle factor is calculated}
  \item{skeleton }{flag: TRUE = a reduced amount of information is calculated yielding a smaller object of type aquaenv}
  \item{K1}{used for TA fitting: give a K\_CO2 and NOT calculate it from T and S: i.e. K\_CO2 can be fitted in the routine as well}
}
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

