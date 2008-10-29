<general>
	<Version>0.2</Version>
	<Date>2008-10-27</Date>
	<Title>AquaEnv - an integrated development toolbox for aquatic chemical model generation</Title>
	<Author>Andreas F. Hofmann</Author>
	<Maintainer>Andreas F. Hofmann <a.hofmann@nioo.knaw.nl></Maintainer>
	<Depends>minpack.lm</Depends>
	<Description>AquaEnv is an integrated development toolbox for aquatic chemical model generation focused on (ocean) acidification and CO2 air-water exchange. 
It contains all elements necessary to model the pH, the related CO2 air-water exchange, as well as aquatic acid-base chemistry in general for
an arbitrary marine, estuarine or freshwater system. Also chemical batches can be modelled. 
Next to the routines necessary to calculate desired information, AquaEnv also contains a suite of tools to visualize this information.
Furthermore, AquaEnv can not only be used to build dynamic models of aquatic systems, but it can also serve as a simple desktop tool for the 
experimental aquatic chemist to generate and visualize all possible derived information from a set of measurements with one single easy to use R function.
Additionally, the sensitivity of the system to variations in the input variables can be visualized.</Description>
	<License>GPL</License>
</general>

<aquaenv>
	<title>aquaenv</title>
	<description>PUBLIC function: the main function of the package AquaEnv: creates an object of class aquaenv</description>
	<usage>aquaenv(Tc, S, d=0, SumCO2=0, SumNH4=0, SumH2S=0, SumH3PO4=0, SumSiOH4=0, SumHNO3=0, SumHNO2=0, SumBOH3=NULL, SumH2SO4=NULL, SumHF=NULL, TA=NULL, pH=NULL, pCO2=NULL, CO2=NULL, speciation=TRUE, dsa=FALSE, ae=NULL, from.data.frame=FALSE, SumH2SO4\_Koffset=0, SumHF\_Koffset=0, revelle=FALSE, skeleton=FALSE)</usage>
	<arguments>
		<Tc>temperature in degrees centigrade</Tc>
		<S>salinity in practical salinity units (i.e. no unit)</S>
		<d>depth in meters</d>
		<SumCO2>total carbonate concentration in mol/kg-solution, if NULL is supplied it is calculated</SumCO2>
		<SumNH4>total ammonium concentration in mol/kg-solution, optional</SumNH4>
		<SumH2S>total sulfide concentration in mol/kg-solution, optional</SumH2S>
		<SumH3PO4>total phosphate concentration in mol/kg-solution, optional</SumH3PO4>
		<SumSiOH4>total silicate concentration in mol/kg-solution, optional</SumSiOH4>
		<SumHNO3>total nitrate concentration in mol/kg-solution, optional</SumHNO3>
		<SumHNO2>total nitrite concentration in mol/kg-solution, optional</SumHNO2>
		<SumBOH3>total borate concentration in mol/kg-solution, calculated from S if not supplied</SumBOH3>
		<SumH2SO4>total sulfate concentration in mol/kg-solution, calculated from S if not supplied</SumH2SO4>
		<SumHF>total fluoride concentration in mol/kg-solution, calculated from S if not supplied</SumHF>
		<TA>total alkalinity in mol/kg-solution, if supplied, pH will be calculated</TA>
		<pH>pH on the free proton concentration scale, if supplied, total alkalinity will be calculated</pH>
		<pCO2>partial pressure of CO2 in atm (note that we do not destinguish between partial pressure and fugacity), can be used with either [TA], pH, or [CO2] to define the system</pCO2>
		<CO2>concentration of CO2, can be used with either [TA], pH, or pCO2 to define the system </CO2>
		<speciation>flag: TRUE = full speciation is calculated</speciation>
		<dsa>flag: TRUE = all information necessary to build a pH model with the direct substitution approach (DSA, Hofmann2008) is calculated</dsa>
		<ae>either an object of class aquaenv used for the cloning functionality or a dataframe used for the from.data.frame functionality</ae>
		<from.data.frame>flag: TRUE = the object of class aquaenv is built from the data frame supplied in ae</from.data.frame>
		<SumH2SO4_Koffset>only used internally to calculate dTAdKdKdSumH2SO4</SumH2SO4_Koffset>
		<SumHF_Koffset>only used internally to calculate dTAdKdKdSumHF</SumHF_Koffset>
		<revelle>flag: TRUE = the revelle factor is calculated</revelle>
		<skeleton>flag: TRUE = a reduced amount of information is calculated yielding a smaller object of type aquaenv</skeleton>
	</arguments>
	<value>
		<Default>a list containing: 
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
		or a subset of this set. Please consult the vignette of AquaEnv for more details</Default>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</aquaenv>

<as.data.frame.aquaenv>
	<title>as.data.frame.aquaenv</title>
	<description>PUBLIC function: converts an object of class aquaenv to a standard R data frame</description>
	<usage>as.data.frame.aquaenv(aquaenv, ...)</usage>
	<arguments>
		<aquaenv>object of type aquaenv</aquaenv>
		<...>further arguments are passed on</...>
	</arguments>
	<value>
		<Default>data frame containing all elements of aquaenv</Default>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</as.data.frame.aquaenv>

<convert>
	<title>convert</title>
	<description>PUBLIC function: high level convert function, calls convert.aquaenv if x is of class aquaenv, or else it calls convert.standard</description>
	<usage>convert(x, ...)</usage>
	<arguments>
		<x>object to be converted</x>
		<...>arguments are passed on: either to convert.aquaenv or convert.standard</...>
	</arguments>
	<value>
		<Default>converted element x</Default>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</convert>

<plot.aquaenv>
	<title>plot.aquaenv</title>
	<description>PUBLIC function: high level plot function for objects of class aquaenv</description>
	<usage>plot.aquaenv(aquaenv, xval, what=NULL, bjerrum=FALSE, cumulative=FALSE, ...)</usage>
	<arguments>
		<aquaenv>object of class aquaenv</aquaenv>
		<xval>a vector of the (maximal) length of the elements of aquaenv against which they are to be plotted</xval>
		<what>a list of names of the elements of aquaenv that are to be plotted, if not supplied and bjerrum=FALSE and cumulative=FALSE: all elements are plotted, if not supplied and bjerrum=TRUE then what is set to be c("CO2", "HCO3", "CO3", "BOH3", "BOH4", "OH", "H3PO4", "H2PO4", "HPO4", "PO4", "SiOH4", "SiOOH3", "SiO2OH2", "H2S", "HS", "S2min", "NH4", "NH3", "H2SO4", "HSO4", "SO4", "HF", "F", "HNO3", "NO3", "HNO2", "NO2"), needs to be supplied for cumulative=TRUE</what>
		<bjerrum>flag: TRUE = a bjerrum plot is done (by calling bjerrumplot)</bjerrum>
		<cumulative>flag: TRUE = a cumulative plot is done (by calling cumulativeplot)</cumulative>
		<...>further arguments are passed on: to plotall, selectplot, bjerrumplot, cumulativeplot</...>
	</arguments>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</plot.aquaenv>

