<general>
	<Version>0.1</Version>
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

<att>
	<title>att</title>
	<description>PRIVATE function: sets the attributes for calculated dissociation constants (Ks)</description>
	<usage>att(K)</usage>
	<arguments>
		<K>the calculated dissociation constant K</K>
	</arguments>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</att>

<deltaPlnK>
	<title>deltaPlnK</title>
	<description>PRIVATE function: the generic function for the pressure correction for dissociation constants and solubility products according to Millero1995</description>
	<usage>deltaPlnK(Tk, d, coeff)</usage>
	<arguments>
		<Tk>temperature in Kelvin</Tk>
		<d>the depth in meters</d>
		<coeff>a vector containing the coefficients a0, a1, a2, b0, b1, b2 for the respective dissociation constant or solubility product</coeff>
	</arguments>
	<references>Millero1995, corrected by Lewis1998</references>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</deltaPlnK>

<Iterms>
	<title>Iterms</title>
	<description>PRIVATE function: returns the ionic strenght I, I(2), sqrt(I), and I*sqrt(I)</description>
	<usage>Iterms(S)</usage>
	<arguments>
		<S>salinity in practical salinity units (i.e. no unit)</S>
	</arguments>
	<value>
		<Default>a list containing:</Default>
		<I>the ionic strength</I>
		<I^2>the square of the ionic strength</I^2>
		<sqrtI>the square root of the ionic strength</sqrtI>
		<I*sqrtI>the ionic strength times its square root</I*sqrtI>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</Iterms>

<K_BOH3>
	<title>K\_BOH3</title>
	<description>PUBLIC function: calculates the dissociation constant of B(OH)3</description>
	<usage>K\_BOH3(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)</usage>
	<arguments>
		<Tc>temperature in degrees centigrade</Tc>
		<S>salinity in practical salinity units (i.e. no unit)</S>
		<d>depth in meters</d>
		<SumH2SO4>total sulfate concentration in mol/kg-solution (calculated from S if not supplied)</SumH2SO4>
		<SumHF>total fluoride concentration in mol/kg-solution (calculated from S if not supplied)</SumHF>
	</arguments>
	<value>
		<Default>the dissociation constant of B(OH)3 in mol/kg-solution on the free proton pH scale</Default>
	</value>
	<references>Dickson1990, DOE1994, Millero1995 (molality version given), Zeebe2001</references>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</K_BOH3>

<K_CO2>
	<title>K\_CO2</title>
	<description>PUBLIC function: calculates the dissociation constant of CO2</description>
	<usage>K\_CO2(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)</usage>
	<arguments>
		<Tc>temperature in degrees centigrade</Tc>
		<S>salinity in practical salinity units (i.e. no unit)</S>
		<d>depth in meters</d>
		<SumH2SO4>total sulfate concentration in mol/kg-solution (calculated from S if not supplied)</SumH2SO4>
		<SumHF>total fluoride concentration in mol/kg-solution (calculated from S if not supplied)</SumHF>
	</arguments>
	<value>
		<Default>the dissociation constant of CO2 in mol/kg-solution on the free proton pH scale</Default>
	</value>
	<references>Roy1993b, DOE1994, Millero1995, Zeebe2001</references>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</K_CO2>

<K_H2PO4>
	<title>K\_H2PO4</title>
	<description>PUBLIC function: calculates the dissociation constant of H2PO4</description>
	<usage>K\_H2PO4(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)</usage>
	<arguments>
		<Tc>temperature in degrees centigrade</Tc>
		<S>salinity in practical salinity units (i.e. no unit)</S>
		<d>depth in meters</d>
		<SumH2SO4>total sulfate concentration in mol/kg-solution (calculated from S if not supplied)</SumH2SO4>
		<SumHF>total fluoride concentration in mol/kg-solution (calculated from S if not supplied)</SumHF>
	</arguments>
	<value>
		<Default>the dissociation constant of H2PO4 in mol/kg-solution on the free proton pH scale</Default>
	</value>
	<references>Millero1995 (original, SWS pH version), DOE1994 (in a later revision cites Millero1995)</references>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</K_H2PO4>

<K_H2S>
	<title>K\_H2S</title>
	<description>PUBLIC function: calculates the dissociation constant of H2S</description>
	<usage>K\_H2S(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)</usage>
	<arguments>
		<Tc>temperature in degrees centigrade</Tc>
		<S>salinity in practical salinity units (i.e. no unit)</S>
		<d>depth in meters</d>
		<SumH2SO4>total sulfate concentration in mol/kg-solution (calculated from S if not supplied)</SumH2SO4>
		<SumHF>total fluoride concentration in mol/kg-solution (calculated from S if not supplied)</SumHF>
	</arguments>
	<value>
		<Default>the dissociation constant of H2S in mol/kg-solution on the free proton pH scale</Default>
	</value>
	<references>Millero1988, Millero1995</references>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</K_H2S>

<K_H3PO4>
	<title>K\_H3PO4</title>
	<description>PUBLIC function: calculates the dissociation constant of H3PO4</description>
	<usage>K\_H3PO4(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)</usage>
	<arguments>
		<Tc>temperature in degrees centigrade</Tc>
		<S>salinity in practical salinity units (i.e. no unit)</S>
		<d>depth in meters</d>
		<SumH2SO4>total sulfate concentration in mol/kg-solution (calculated from S if not supplied)</SumH2SO4>
		<SumHF>total fluoride concentration in mol/kg-solution (calculated from S if not supplied)</SumHF>
	</arguments>
	<value>
		<Default>the dissociation constant of H3PO4 in mol/kg-solution on the free proton pH scale</Default>
	</value>
	<references>Millero1995 (original, SWS pH version), DOE1994 (in a later revision cites Millero1995)</references>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</K_H3PO4>

<K_HCO3>
	<title>K\_HCO3</title>
	<description>PUBLIC function: calculates the dissociation constant of HCO3</description>
	<usage>K\_HCO3(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)</usage>
	<arguments>
		<Tc>temperature in degrees centigrade</Tc>
		<S>salinity in practical salinity units (i.e. no unit)</S>
		<d>depth in meters</d>
		<SumH2SO4>total sulfate concentration in mol/kg-solution (calculated from S if not supplied)</SumH2SO4>
		<SumHF>total fluoride concentration in mol/kg-solution (calculated from S if not supplied)</SumHF>
	</arguments>
	<value>
		<Default>the dissociation constant of HCO3 in mol/kg-solution on the free proton pH scale</Default>
	</value>
	<references>Roy1993b, DOE1994, Millero1995, Zeebe2001</references>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</K_HCO3>

<K_HF>
	<title>K\_HF</title>
	<description>PUBLIC function: calculates the dissociation constant of HF</description>
	<usage>K\_HF(Tc, S, d=0)</usage>
	<arguments>
		<Tc>temperature in degrees centigrade</Tc>
		<S>salinity in practical salinity units (i.e. no unit)</S>
		<d>depth in meters</d>
	</arguments>
	<value>
		<Default>the dissociation constant of HF in mol/kg-solution on the free proton pH scale</Default>
	</value>
	<references>Dickson1979a, Dickson1987, Roy1993b, DOE1994, Millero1995, Zeebe2001</references>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</K_HF>

<K_HPO4>
	<title>K\_HPO4</title>
	<description>PUBLIC function: calculates the dissociation constant of HPO4</description>
	<usage>K\_HPO4(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)</usage>
	<arguments>
		<Tc>temperature in degrees centigrade</Tc>
		<S>salinity in practical salinity units (i.e. no unit)</S>
		<d>depth in meters</d>
		<SumH2SO4>total sulfate concentration in mol/kg-solution (calculated from S if not supplied)</SumH2SO4>
		<SumHF>total fluoride concentration in mol/kg-solution (calculated from S if not supplied)</SumHF>
	</arguments>
	<value>
		<Default>the dissociation constant of HPO4 in mol/kg-solution on the free proton pH scale</Default>
	</value>
	<references>Millero1995 (original, SWS pH version), DOE1994 (in a later revision cites Millero1995)</references>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</K_HPO4>

<K_HSO4>
	<title>K\_HSO4</title>
	<description>PUBLIC function: calculates the dissociation constant of HSO4</description>
	<usage>K\_HSO4(Tc, S, d=0)</usage>
	<arguments>
		<Tc>temperature in degrees centigrade</Tc>
		<S>salinity in practical salinity units (i.e. no unit)</S>
		<d>depth in meters</d>
	</arguments>
	<value>
		<Default>the dissociation constant of HSO4 in mol/kg-solution on the free proton pH scale</Default>
	</value>
	<references>Dickson1990, DOE1994, Zeebe2001</references>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</K_HSO4>

<K_NH4>
	<title>K\_NH4</title>
	<description>PUBLIC function: calculates the dissociation constant of NH4</description>
	<usage>K\_NH4(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)</usage>
	<arguments>
		<Tc>temperature in degrees centigrade</Tc>
		<S>salinity in practical salinity units (i.e. no unit)</S>
		<d>depth in meters</d>
		<SumH2SO4>total sulfate concentration in mol/kg-solution (calculated from S if not supplied)</SumH2SO4>
		<SumHF>total fluoride concentration in mol/kg-solution (calculated from S if not supplied)</SumHF>
	</arguments>
	<value>
		<Default>the dissociation constant of NH4 in mol/kg-solution on the free proton pH scale</Default>
	</value>
	<references>Millero1995a, Millero1995, corrected by Lewis1998</references>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</K_NH4>

<K_SiOH4>
	<title>K\_SiOH4</title>
	<description>PUBLIC function: calculates the dissociation constant of SiOH4</description>
	<usage>K\_SiOH4(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)</usage>
	<arguments>
		<Tc>temperature in degrees centigrade</Tc>
		<S>salinity in practical salinity units (i.e. no unit)</S>
		<d>depth in meters</d>
		<SumH2SO4>total sulfate concentration in mol/kg-solution (calculated from S if not supplied)</SumH2SO4>
		<SumHF>total fluoride concentration in mol/kg-solution (calculated from S if not supplied)</SumHF>
	</arguments>
	<value>
		<Default>the dissociation constant of SiOH4 in mol/kg-solution on the free proton pH scale</Default>
	</value>
	<references>Millero1988, DOE1994, Millero1995</references>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</K_SiOH4>

<K_SiOOH3>
	<title>K\_SiOOH3</title>
	<description>PUBLIC function: calculates the dissociation constant of SiOOH3</description>
	<usage>K\_SiOOH3(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)</usage>
	<arguments>
		<Tc>temperature in degrees centigrade</Tc>
		<S>salinity in practical salinity units (i.e. no unit)</S>
		<d>depth in meters</d>
		<SumH2SO4>total sulfate concentration in mol/kg-solution (calculated from S if not supplied)</SumH2SO4>
		<SumHF>total fluoride concentration in mol/kg-solution (calculated from S if not supplied)</SumHF>
	</arguments>
	<value>
		<Default>the dissociation constant of SiOOH3 in mol/kg-solution on the free proton pH scale</Default>
	</value>
	<references>Wischmeyer2003 (incl. corrections)</references>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</K_SiOOH3>

<K_W>
	<title>K\_W</title>
	<description>PUBLIC function: calculates the ion product of H2O</description>
	<usage>K\_W(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)</usage>
	<arguments>
		<Tc>temperature in degrees centigrade</Tc>
		<S>salinity in practical salinity units (i.e. no unit)</S>
		<d>depth in meters</d>
		<SumH2SO4>total sulfate concentration in mol/kg-solution (calculated from S if not supplied)</SumH2SO4>
		<SumHF>total fluoride concentration in mol/kg-solution (calculated from S if not supplied)</SumHF>
	</arguments>
	<value>
		<Default>the ion product of H2O in (mol/kg-solution)2 on the free proton pH scale</Default>
	</value>
	<references>Millero1995 (SWS pH version), DOE1994 (cites Millero1995), Zeebe2001</references>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</K_W>

<K0_CO2>
	<title>K0\_CO2</title>
	<description>PUBLIC function: calculates the Henry's constant (solubility) for CO2</description>
	<usage>K0\_CO2(Tc, S)</usage>
	<arguments>
		<Tc>temperature in degrees centigrade</Tc>
		<S>salinity in practical salinity units (i.e. no unit)</S>
	</arguments>
	<value>
		<Default>the Henry's constant for CO2 in mol/(kg-solution*atm)</Default>
	</value>
	<references>Weiss1974, DOE1994, Millero1995, Zeebe2001</references>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</K0_CO2>

<K0_O2>
	<title>K0\_O2</title>
	<description>PUBLIC function: calculates the Henry's constant (solubility) for O2</description>
	<usage>K0\_O2(Tc, S)</usage>
	<arguments>
		<Tc>temperature in degrees centigrade</Tc>
		<S>salinity in practical salinity units (i.e. no unit)</S>
	</arguments>
	<value>
		<Default>the Henry's constant for CO2 in mol/(kg-solution*atm)</Default>
	</value>
	<references>derived from a formulation for [O2]sat given in Weiss1970</references>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</K0_O2>

<Ksp_aragonite>
	<title>Ksp\_aragonite</title>
	<description>PUBLIC function: calculates the solubility product for aragonite</description>
	<usage>Ksp\_aragonite(Tc, S, d=0)</usage>
	<arguments>
		<Tc>temperature in degrees centigrade</Tc>
		<S>salinity in practical salinity units (i.e. no unit)</S>
		<d>depth in meters</d>
	</arguments>
	<value>
		<Default>the solubility product for aragonite in (mol/kg-solution)2</Default>
	</value>
	<references>Mucci1983, Boudreau1996</references>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</Ksp_aragonite>

<Ksp_calcite>
	<title>Ksp\_calcite</title>
	<description>PUBLIC function: calculates the solubility product for aragonite</description>
	<usage>Ksp\_calcite(Tc, S, d=0)</usage>
	<arguments>
		<Tc>temperature in degrees centigrade</Tc>
		<S>salinity in practical salinity units (i.e. no unit)</S>
		<d>depth in meters</d>
	</arguments>
	<value>
		<Default>the solubility product for calcite in (mol/kg-solution)2</Default>
	</value>
	<references>Mucci1983, Boudreau1996</references>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</Ksp_calcite>

<lnK>
	<title>lnK</title>
	<description>PRIVATE function: generic formula (see publication associated with AquaEnv) for K calculations that use the natural logarithm (ln)</description>
	<usage>lnK(A, B, C, D, E, Tk)</usage>
	<arguments>
		<A>coefficient A</A>
		<B>coefficient B</B>
		<C>coefficient C</C>
		<D>coefficient D</D>
		<E>coefficient E</E>
		<Tk>temperature in Kelvin</Tk>
	</arguments>
	<value>
		<Default>the ln of the K associated with the coefficients</Default>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</lnK>

<logK>
	<title>logK</title>
	<description>PRIVATE function: generic formula (see publication associated with AquaEnv) for K calculations that use the decadal logarithm (log)</description>
	<usage>logK(A, B, C, D, E, Tk)</usage>
	<arguments>
		<A>coefficient A</A>
		<B>coefficient B</B>
		<C>coefficient C</C>
		<D>coefficient D</D>
		<E>coefficient E</E>
		<Tk>temperature in Kelvin</Tk>
	</arguments>
	<value>
		<Default>the log of the K associated with the coefficients</Default>
	</value>	
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</logK>

<splitS_K_CO2>
	<title>splitS\_K\_CO2</title>
	<description>PRIVATE function: returns the intersection of the formulae for K\_CO2 for S < 5 and S >= 5</description>
	<usage>splitS\_K\_CO2(Tk)</usage>
	<arguments>
		<Tk>temperature in Kelvin</Tk>
	</arguments>
	<value>
		<Default>the value for S where the two formulae intersect at temperature Tk</Default>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</splitS_K_CO2>

<splitS_K_HCO3>
	<title>splitS\_K\_HCO3</title>
	<description>PRIVATE function: returns the intersection of the formulae for K\_HCO3 for S < 5 and S >= 5</description>
	<usage>splitS\_K\_HCO3(Tk)</usage>
	<arguments>
		<Tk>temperature in Kelvin</Tk>
	</arguments>
	<value>
		<Default>the value for S where the two formulae intersect at temperature Tk</Default>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</splitS_K_HCO3>

<Sterms>
	<title>Sterms</title>
	<description>PRIVATE function: returns S, S(2), sqrt(S), and S*sqrt(S)</description>
	<usage>Sterms(S)</usage>
	<arguments>
		<S>salinity in practical salinity units (i.e. no unit)</S>
	</arguments>
	<value>
		<Default>a list containing:</Default>
		<S^2>the square of S</S^2>
		<sqrtS>the square root of S</sqrtS>
		<S*sqrtS>S times its square root</S*sqrtS>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</Sterms>
