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

<basicplot>
	<title>basicplot</title>
	<description>PRIVATE function: bbasic wrapper for the R plot function for plotting objects of class aquaenv; no return value, just side-effect</description>
	<usage>basicplot(aquaenv, xval, type="l", mgp=c(1.8, 0.5, 0), mar=c(3,3,0.5,0.5), oma=c(0,0,0,0), size=c(15,13), mfrow=c(11,10), device="x11", filename="aquaenv", ...)</usage>
	<arguments>
		<aquaenv>object of class aquaenv</aquaenv>
		<xval>x-value: the independent variable describing a change in elements of an object of class aquaenv</xval>
		<type>standard plot parameter;     default: plot lines</type>
		<mgp>standard plot parameter;     default: axis title on line 1.8, axis labels on line 0.5, axis on line 0</mgp>
		<mar>standard plot parameter;     default: margin of 3 lines bottom and left and 0.5 lines top and right</mar>
		<oma>standard plot parameter;     default: no outer margin</oma>
		<size>the size of the plot device; default: 15 (width) by 13 (height) inches</size>
		<mfrow>standard plot parameter;     default: 11 columns and 10 rows of plots</mfrow>
		<device>the device to plot on;       default: "x11" (can also be "eps" or "pdf")</device>
		<filename>filename to be used if "eps" or "pdf" is selected for device</filename>
		<...>further arguments will be passed</...>
	</arguments>
	<author>Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)</author>
	<keyword>IO</keyword>
</basicplot>

<bjerrumplot>
	<title>bjerrumplot</title>
	<description>PRIVATE function: creates a bjerrumplot from the elements of an object of class aquaenv given in what; no return value, just side-effect</description>
	<usage>bjerrumplot(aquaenv, what, log=FALSE, palette=NULL, device="x11", filename="aquaenv", size=c(12,10), ylim=NULL, lwd=2, xlab="free scale pH", mgp=c(1.8, 0.5, 0), mar=c(3,3,0.5,0.5), oma=c(0,0,0,0), legendposition="bottomleft", legendinset=0.05, legendlwd=4, bg="white", ...)</usage>
	<arguments>
		<aquaenv>object of class aquaenv</aquaenv>
		<what>vector of names of elements of aquaenv that should be plotted; if not specified:  what <- c("CO2", "HCO3", "CO3", "BOH3", "BOH4", "OH", "H3PO4", "H2PO4", "HPO4", "PO4", "SiOH4", "SiOOH3", "SiO2OH2", "H2S", "HS", "S2min", "NH4", "NH3", "H2SO4", "HSO4", "SO4", "HF", "F", "HNO3", "NO3", "HNO2", "NO2")</what>
		<log>should the plot be on a logarithmic y axis?</log>
		<palette>a vector of colors to use in the plot (either numbers or names given in colors())</palette>
		<device>the device to plot on; default: "x11" (can also be "eps" or "pdf")</device>
		<filename>filename to be used if "eps" or "pdf" is selected for device</filename>
		<size>the size of the plot device; default: 12 (width) by 10 (height) inches</size>
		<ylim>standard plot parameter; if not supplied it will be calculated by range() of the elements to plot</ylim>
		<lwd>standard plot parameter; width of the lines in the plot</lwd>
		<xlab>x axis label</xlab>
		<mgp>standard plot parameter; default: axis title on line 1.8, axis labels on line 0.5, axis on line 0</mgp>
		<mar>standard plot parameter; default: margin of 3 lines bottom and left and 0.5 lines top and right</mar>
		<oma>standard plot parameter; default: no outer margin</oma>
		<legendposition>position of the legend</legendposition>
		<legendinset>standard legend parameter inset</legendinset>
		<legendlwd>standard legend parameter lwd: line width of lines in legend</legendlwd>
		<bg>standard legend parameter: default background color: white</bg>
		<...>further arguments will be passed</...>
	</arguments>
	<author>Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)</author>
	<keyword>IO</keyword>
</bjerrumplot>

<c.aquaenv>
	<title>c.aquaenv</title>
	<description>PRIVATE function: adds an element to an object of class aquaenv</description>
	<usage>c.aquaenv(aquaenv, x, ...)</usage>
	<arguments>
		<aquaenv>object of class aquaenv</aquaenv>
		<x>a vector of the form c(value, name) representing the element to be inserted into the object of class aquaenv</x>
		<...>further arguments will be passed</...>
	</arguments>
	<value>
		<Default>object of class aquaenv with the added element</Default>
	</value>
	<author>Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)</author>
	<keyword>misc</keyword>
</c.aquaenv>

<Cl>
	<title>Cl</title>
	<description>PRIVATE function: calculates chlorinity Cl from salinity S</description>
	<usage>Cl(S)</usage>
	<arguments>
		<S>salinity S in practical salinity units (i.e. no unit)</S>
	</arguments>
	<value>
		<Default>chlorinity Cl in permil</Default>
	</value>
	<author>Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)</author>
	<references>DOE1994, Zeebe2001</references>
	<keyword>misc</keyword>
</Cl>

<cloneaquaenv>
	<title>cloneaquaenv</title>
	<description>PRIVATE function: clones an object of class aquaenv: it is possible to supply a new value for either TA or pH; the switches speciation, skeleton, revelle, and dsa are obtained from the object to be cloned</description>
	<usage>cloneaquaenv(aquaenv, TA=NULL, pH=NULL)</usage>
	<arguments>
		<aquaenv>object of class aquaenv</aquaenv>
		<TA>optional new value for TA</TA>
		<pH>optional new value for pH</pH>
	</arguments>
	<value>
		<Default>cloned object of class aquaenv</Default>
	</value>
	<author>Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)</author>
	<keyword>misc</keyword>
</cloneaquaenv>

<convert.aquaenv>
	<title>convert.aquaenv</title>
	<description>PRIVATE function: converts all elements of a special unit or pH scale in an object of class aquaenv</description>
	<usage>convert.aquaenv(aquaenv, from, to, factor, convattr="unit", ...)</usage>
	<arguments>
		<aquaenv>object of class aquaenv</aquaenv>
		<from>the unit which needs to be converted (as a string; must be a perfect match)</from>
		<to>the unit to which the conversion should go</to>
		<factor>the conversion factor to be applied: can either be a number (e.g. 1000 to convert from mol to mmol), or any of the conversion factors given in an object of class  aquaenv</factor>
		<convattr>which attribute should be converted? can either be "unit" or "pH scale"</convattr>
		<...>further arguments will be passed</...>
	</arguments>
	<value>
		<Default>object of class aquaenv whith the converted elements</Default>
	</value>
	<author>Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)</author>
	<keyword>misc</keyword>
</convert.aquaenv>

<convert.standard>
	<title>convert.standard</title>
	<description>PRIVATE function: converts either the pH scale of a pH value, the pH scale of a dissociation constant (K*), or the unit of a concentration value</description>
	<usage>convert.standard(x, vartype, what, Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)</usage>
	<arguments>
		<x>the object to be converted (pH value, K* value, or concentration value)</x>
		<vartype>the type of x, either "pHscale", "KHscale", or "conc"</vartype>
		<what>the type of conversion to be done, for pH scales one of "free2tot", "free2sws", "free2nbs", ... (any combination of "free", "tot", "sws", and "nbs"); for concentrations one of "molar2molal", "molar2molin", ... (any combination of "molar" (mol/l), "molal" (mol/kg-H2O), and "molin" (mol/kg-solution))</what>
		<Tc>temperature in degrees centigrade</Tc>
		<S>salinity (in practical salinity units: no unit)</S>
		<d>depth in meters</d>
		<SumH2SO4>total sulfate concentration in mol/kg-solution; if not supplied this is calculated from S</SumH2SO4>
		<SumHF>total fluoride concentration in mol/kg-solution; if not supplied this is calculated from S</SumHF>
	</arguments>
	<value>
		<Default>converted pH, K*, or concentration value, attributed with the new unit/pH scale</Default>
	</value>
	<author>Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)</author>
	<keyword>misc</keyword>
</convert.standard>

<cumulativeplot>
	<title>cumulativeplot</title>
	<description>PRIVATE function: creates a cumulative plot from the elements of an object of class aquaenv given in what; no return value, just side-effect</description>
	<usage>cumulativeplot(aquaenv, xval, what, total=TRUE, palette=NULL, device="x11", filename="aquaenv", size=c(12,10), ylim=NULL, lwd=2, mgp=c(1.8, 0.5, 0), mar=c(3,3,0.5,0.5), oma=c(0,0,0,0), legendposition="bottomleft", legendinset=0.05, legendlwd=4, bg="white", y.intersp=1.2, ...)</usage>
	<arguments>
		<aquaenv>object of class aquaenv</aquaenv>
		<xval>x-value: the independent variable describing a change in elements of an object of class aquaenv</xval>
		<what>vector of names of elements of aquaenv that should be plotted</what>
		<total>should the sum of all elements specified in what be plotted as well?</total>
		<palette>a vector of colors to use in the plot (either numbers or names given in colors())</palette>
		<device>the device to plot on;       default: "x11" (can also be "eps" or "pdf")</device>
		<filename>filename to be used if "eps" or "pdf" is selected for device</filename>
		<size>the size of the plot device; default: 12 (width) by 10 (height) inches</size>
		<ylim>standard plot parameter; if not supplied it will be calculated by an adaptation of range() of the elements to plot</ylim>
		<lwd>standard plot parameter; width of the lines in the plot</lwd>
		<mgp>standard plot parameter; default: axis title on line 1.8, axis labels on line 0.5, axis on line 0</mgp>
		<mar>standard plot parameter; default: margin of 3 lines bottom and left and 0.5 lines top and right</mar>
		<oma>standard plot parameter; default: no outer margin</oma>
		<legendposition>position of the legend</legendposition>
		<legendinset>standard legend parameter inset</legendinset>
		<legendlwd>standard legend parameter lwd: line width of lines in legend</legendlwd>
		<bg>standard legend parameter: default background color: white</bg>
		<y.intersp>standard legend parameter; default: 1.2 lines space between the lines in the legend</y.intersp>
		<...>further arguments will be passed</...>
	</arguments>
	<author>Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)</author>
	<keyword>IO</keyword>
</cumulativeplot>

<from.data.frame>
	<title>from.data.frame</title>
	<description>PRIVATE function: creates an object of class aquaenv from a data frame (e.g. as supplied from the numerical solver of a dynamic model)</description>
	<usage>from.data.frame(df)</usage>
	<arguments>
		<df>data frame</df>
	</arguments>
	<value>
		<Default>object of class aquaenv</Default>
	</value>
	<author>Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)</author>
	<keyword>misc</keyword>
</from.data.frame>

<hydroP>
	<title>hydroP</title>
	<description>PRIVATE function: calculates the hydrostatic pressure from the depth (the hydrostatic pressure increases per m depth by 1/10 of 1 atm)</description>
	<usage>hydroP(d)</usage>
	<arguments>
		<d>depth in meters</d>
	</arguments>
	<value>
		<Default>hydrostatic pressure in bars</Default>
	</value>
	<author>Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)</author>
	<keyword>misc</keyword>
</hydroP>

<I>
	<title>I</title>
	<description>PRIVATE function: calculates the ionic strength I as a function of salinity S</description>
	<usage>I(S)</usage>
	<arguments>
		<S>salinity S in practical salinity units (i.e. no unit)</S>
	</arguments>
	<value>
		<Default>ionic strength in mol/kg-solution (molinity)</Default>
	</value>
	<author>Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)</author>
	<references>DOE1994, Zeebe2001, Roy1993b (the carbonic acid paper)</references>
	<keyword>misc</keyword>
</I>

<length.aquaenv>
	<title>length.aquaenv</title>
	<description>PRIVATE function: returns the (maximal) length of the elements in an object of class aquaenv (i.e. > 1 if one of the input variables was a vector)</description>
	<usage>length.aquaenv(aquaenv, ...)</usage>
	<arguments>
		<aquaenv>object of class aquaenv</aquaenv>
		<...>further arguments will be passed</...>
	</arguments>
	<value>
		<Default>the maximal length of the elements in the object of class aquaenv</Default>
	</value>
	<author>Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)</author>
	<keyword>misc</keyword>
</length.aquaenv>

<merge.aquaenv>
	<title>merge.aquaenv</title>
	<description>PRIVATE function: merges the elements of two objects of class aquaenv: element names are taken from the first argument, the elements of which are also first in the merged object</description>
	<usage>merge.aquaenv(aquaenv1, aquaenv2, ...)</usage>
	<arguments>
		<aquaenv1>object of class aquaenv: this is where the element names are taken from</aquaenv1>
		<aquaenv2>object of class aquaenv: must contain at leas all the element (names) as aquaenv1, extra elements are ignored</aquaenv2>
		<...>further arguments will be passed</...>
	</arguments>
	<value>
		<Default>object of class aquaenv with merged elements</Default>
	</value>
	<author>Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)</author>
	<keyword>misc</keyword>
</merge.aquaenv>

<molal2molin>
	<title>molal2molin</title>
	<description>PRIVATE function: calculates the conversion factor converting from molality (mol/kg-H2O) to molinity (mol/kg-solution) from salinity S</description>
	<usage>molal2molin(S)</usage>
	<arguments>
		<S>salinity S in practical salinity units (i.e. no unit)</S>
	</arguments>
	<value>
		<Default>the conversion factor from molality (mol/kg-H2O) to molinity (mol/kg-solution)</Default>
	</value>
	<author>Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)</author>
	<references>Roy1993b (the carbonic acid paper), DOE1994</references>
	<keyword>misc</keyword>
</molal2molin>

<opendevice>
	<title>opendevice</title>
	<description>PRIVATE function: opens a device for plotting; no return value, just side-effect</description>
	<usage>opendevice(device, size, filename)</usage>
	<arguments>
		<device>either "x11", "eps", or "pdf"</device>
		<size>size of the plot device in the form c(width, height)</size>
		<filename>filename to use if "eps" or "pdf" is used</filename>
	</arguments>
	<author>Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)</author>
	<keyword>IO</keyword>
</opendevice>

<plotall>
	<title>plotall</title>
	<description>PRIVATE function: plots all elements of an object of class aquaenv; no return value, just side-effect</description>
	<usage>plotall(aquaenv, xval, ...)</usage>
	<arguments>
		<aquaenv>object of class aquaenv</aquaenv>
		<xval>x-value: the independent variable describing a change in elements of an object of class aquaenv</xval>
		<...>further arguments will be passed</...>
	</arguments>
	<author>Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)</author>
	<keyword>IO</keyword>
</plotall>

<scaleconvert>
	<title>scaleconvert</title>
	<description>PRIVATE function: provides pH scale conversion factors (caution: the activity coefficient for H+ (needed for NBS scale conversions) is calculated with the Davies equation (Zeebe2001) which is only accurate up to ionic strengthes of I = 0.5)</description>
	<usage>scaleconvert(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)</usage>
	<arguments>
		<Tc>temperature in degrees centigrade</Tc>
		<S>salinity S in practical salinity units (i.e. no unit)</S>
		<d>depth in meters</d>
		<SumH2SO4>total sulfate concentration in mol/kg-solution; if not supplied this is calculated from S</SumH2SO4>
		<SumHF>total fluoride concentration in mol/kg-solution; if not supplied this is calculated from S</SumHF>
	</arguments>
	<value>
		<Default>a list of conversion factors "free2tot", "free2sws", etc.</Default>
	</value>
	<author>Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)</author>
	<references>Dickson1984, DOE1994, Zeebe2001</references>
	<keyword>misc</keyword>
</scaleconvert>

<seaconc>
	<title>seaconc</title>
	<description>PRIVATE function: calculates concentrations of constituents of natural seawater from a given salinity S</description>
	<usage>seaconc(spec, S)</usage>
	<arguments>
		<spec>constituent of seawater (chemical species) of which the concentration should be calculated. can be any name of the vectors ConcRelCl and MeanMolecularWeight: "Cl", "SO4", "Br", "F", "Na", "Mg", "Ca", "K", "Sr", "B", "S"</spec>
		<S>salinity S in practical salinity units (i.e. no unit)</S>
	</arguments>
	<value>
		<Default>concentration of the constituent of seawater speciefied in spec in mol/kg-solution (molinity): this is determined by the data in ConcRelCl and MeanMolecularWeight</Default>
	</value>
	<author>Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)</author>
	<references>DOE1994</references>
	<keyword>misc</keyword>
</seaconc>

<seadensity>
	<title>seadensity</title>
	<description>PRIVATE function: calculates seawater density (in kg/m3) from temperature (in degrees centigrade) and salinity</description>
	<usage>seadensity(Tc, S)</usage>
	<arguments>
		<Tc>temperature in degrees centigrade</Tc>
		<S>salinity S in practical salinity units (i.e. no unit)</S>
	</arguments>
	<value>
		<Default>seawater density in kg/m3</Default>
	</value>
	<author>Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)</author>
	<references>Millero1981, DOE1994</references>
	<keyword>misc</keyword>
</seadensity>

<selectplot>
	<title>selectplot</title>
	<description>PRIVATE function: plots just the elements of an object of class aquaenv given in what; no return value, just side-effect</description>
	<usage>selectplot(aquaenv, xval, what, mfrow=c(1,1), size=c(7,7), ...)</usage>
	<arguments>
		<aquaenv>object of class aquaenv</aquaenv>
		<xval>x-value: the independent variable describing a change in elements of an object of class aquaenv</xval>
		<what>vector of names of elements of aquaenv that should be plotted</what>
		<mfrow>standard plot parameter; default: just one plot</mfrow>
		<size>the size of the plot device; default: 7 (width) by 7 (height) inches</size>
		<...>further arguments will be passed</...>
	</arguments>
	<author>Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)</author>
	<keyword>IO</keyword>
</selectplot>

<Tk>
	<title>Tk</title>
	<description>PRIVATE function: calculates the temperature in Kelvin from the temperature in degrees centigrade</description>
	<usage>Tk(Tc)</usage>
	<arguments>
		<Tc>temperature in degrees centigrade</Tc>
	</arguments>
	<value>
		<Default>temperature in Kelvin</Default>
	</value>
	<author>Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)</author>
	<keyword>misc</keyword>
</Tk>

