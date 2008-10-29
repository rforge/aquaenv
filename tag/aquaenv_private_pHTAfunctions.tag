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

<Abi>
	<title>Abi</title>
	<description>PRIVATE function: calculates [A(2-)] of a bivalent acid</description>
	<usage>Abi(Sum, K1, K2, H)</usage>
	<arguments>
		<Sum>the total concentration of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</Sum>
		<K1>the first dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K1>
		<K2>the second dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K2>
		<H>the proton concentration in a unit consistent with all othert input variables (e.g. mol/kg-solution)</H>
	</arguments>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</Abi>

<Atri>
	<title>Atri</title>
	<description>PRIVATE function: calculates [A(3-)] of a trivalent acid</description>
	<usage>Atri(Sum, K1, K2, K3, H)</usage>
	<arguments>
		<Sum>the total concentration of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</Sum>
		<K1>the first dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K1>
		<K2>the second dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K2>
		<K3>the third dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K3>
		<H>the proton concentration in a unit consistent with all othert input variables (e.g. mol/kg-solution)</H>
	</arguments>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</Atri>

<Auni>
	<title>Auni</title>
	<description>PRIVATE function: calculates [A(-)] of an univalent acid</description>
	<usage>Auni(Sum, K, H)</usage>
	<arguments>
		<Sum>the total concentration of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</Sum>
		<K>the dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K>
		<H>the proton concentration in a unit consistent with all othert input variables (e.g. mol/kg-solution)</H>
	</arguments>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</Auni>

<calcH_CO2>
	<title>calcH\_CO2</title>
	<description>PRIVATE function: calculates [H+]  from an object of class aquanenv and a given [CO2]: by analytically solving the resulting quadratic equation</description>
	<usage>calcH_CO2(aquaenv, CO2)</usage>
	<arguments>
		<aquaenv>object of class aquaenv</aquaenv>
		<CO2>given [CO2] in mol/kg-solution</CO2>
	</arguments>
	<value>
		<Default>calculated [H+] in mol/kg-solution</Default>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</calcH_CO2>

<calcH_TA>
	<title>calcH\_TA</title>
	<description>PRIVATE function: calculates [H+]  from an object of class aquanenv and a given [TA]: first according to Follows2006, if no solution is found after Technicals\$maxiter iterations, uniroot is applied</description>
	<usage>calcH_TA(aquaenv, TA)</usage>
	<arguments>
		<aquaenv>object of class aquaenv</aquaenv>
		<TA>given [TA] in mol/kg-solution</TA>
	</arguments>
	<value>
		<Default>calculated [H+] in mol/kg-solution</Default>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</calcH_TA>

<calcSumCO2_pH_CO2>
	<title>calcSumCO2\_pH\_CO2</title>
	<description>PRIVATE function: calculates [SumCO2]  from an object of class aquanenv, a given pH, and a given [CO2]: by analytically solving the resulting equation</description>
	<usage>calcSumCO2_pH_CO2(aquaenv, pH, CO2)</usage>
	<arguments>
		<aquaenv>object of class aquaenv</aquaenv>
		<pH>given pH on the free proton scale</pH>
		<CO2>given [CO2] in mol/kg-solution</CO2>
	</arguments>
	<value>
		<Default>calculated [SumCO2] in mol/kg-solution</Default>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</calcSumCO2_pH_CO2>

<calcSumCO2_pH_TA>
	<title>calcSumCO2\_pH\_TA</title>
	<description>PRIVATE function: calculates [SumCO2]  from an object of class aquanenv, a given pH, and a given [TA]: by analytically solving the resulting quadratic equation</description>
	<usage>calcSumCO2_pH_TA(aquaenv, pH, TA)</usage>
	<arguments>
		<aquaenv>object of class aquaenv</aquaenv>
		<pH>given pH on the free proton scale</pH>
		<TA>given [TA] in mol/kg-solution</TA>
	</arguments>
	<value>
		<Default>calculated [SumCO2] in mol/kg-solution</Default>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</calcSumCO2_pH_TA>

<calcSumCO2_TA_CO2>
	<title>calcSumCO2\_TA\_CO2</title>
	<description>PRIVATE function: calculates [SumCO2] from an object of class aquanenv, a given [TA], and a given [CO2]: by analytically solving the resulting quadratic equation</description>
	<usage>calcSumCO2_TA_CO2(aquaenv, TA, CO2)</usage>
	<arguments>
		<aquaenv>object of class aquaenv</aquaenv>
		<TA>given [TA] in mol/kg-solution</TA>
		<CO2>given [CO2] in mol/kg-solution</CO2>
	</arguments>
	<value>
		<Default>calculated [SumCO2] in mol/kg-solution</Default>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</calcSumCO2_TA_CO2>

<calcTA>
	<title>calcTA</title>
	<description>PRIVATE function: calculates [TA] from an object of class aquanenv and a given [H+]</description>
	<usage>calcTA(aquaenv, H)</usage>
	<arguments>
		<aquaenv>object of class aquaenv</aquaenv>
		<H>the proton concentration in a unit consistent with all othert input variables (e.g. mol/kg-solution)given [H+] in mol/kg-solution</H>
	</arguments>
	<value>
		<Default>the calculated [TA]</Default>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</calcTA>

<calcTAMinor>
	<title>calcTAMinor</title>
	<description>PRIVATE function: calculates minor contributions to [TA] from an object of class aquanenv and a given [H+]</description>
	<usage>calcTAMinor(aquaenv, H)</usage>
	<arguments>
		<aquaenv>object of class aquaenv</aquaenv>
		<H>the proton concentration in a unit consistent with all othert input variables (e.g. mol/kg-solution)given [H+] in mol/kg-solution</H>
	</arguments>
	<value>
		<Default>calculated minor contributions to [TA]</Default>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</calcTAMinor>

<H2Abi>
	<title>H2Abi</title>
	<description>PRIVATE function: calculates [H2A] of a bivalent acid</description>
	<usage>H2Abi(Sum, K1, K2, H)</usage>
	<arguments>
		<Sum>the total concentration of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</Sum>
		<K1>the first dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K1>
		<K2>the second dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K2>
		<H>the proton concentration in a unit consistent with all othert input variables (e.g. mol/kg-solution)</H>
	</arguments>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</H2Abi>

<H2Atri>
	<title>H2Atri</title>
	<description>PRIVATE function: calculates [H2A(-)] of a trivalent acid</description>
	<usage>H2Atri(Sum, K1, K2, K3, H)</usage>
	<arguments>
		<Sum>the total concentration of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</Sum>
		<K1>the first dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K1>
		<K2>the second dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K2>
		<K3>the third dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K3>
		<H>the proton concentration in a unit consistent with all othert input variables (e.g. mol/kg-solution)</H>
	</arguments>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</H2Atri>

<H3Atri>
	<title>H3Atri</title>
	<description>PRIVATE function: calculates [H3A] of a trivalent acid</description>
	<usage>H3Atri(Sum, K1, K2, K3, H)</usage>
	<arguments>
		<Sum>the total concentration of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</Sum>
		<K1>the first dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K1>
		<K2>the second dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K2>
		<K3>the third dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K3>
		<H>the proton concentration in a unit consistent with all othert input variables (e.g. mol/kg-solution)</H>
	</arguments>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</H3Atri>

<HAbi>
	<title>HAbi</title>
	<description>PRIVATE function: calculates [HA(-)] of a bivalent acid</description>
	<usage>HAbi(Sum, K1, K2, H)</usage>
	<arguments>
		<Sum>the total concentration of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</Sum>
		<K1>the first dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K1>
		<K2>the second dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K2>
		<H>the proton concentration in a unit consistent with all othert input variables (e.g. mol/kg-solution)</H>
	</arguments>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</HAbi>

<HAtri>
	<title>HAtri</title>
	<description>PRIVATE function: calculates [HA(2-)] of a trivalent acid</description>
	<usage>HAtri(Sum, K1, K2, K3, H)</usage>
	<arguments>
		<Sum>the total concentration of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</Sum>
		<K1>the first dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K1>
		<K2>the second dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K2>
		<K3>the third dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K3>
		<H>the proton concentration in a unit consistent with all othert input variables (e.g. mol/kg-solution)</H>
	</arguments>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</HAtri>

<HAuni>
	<title>HAuni</title>
	<description>PRIVATE function: calculates [HA] of an univalent acid</description>
	<usage>HAuni(Sum, K, H)</usage>
	<arguments>
		<Sum>the total concentration of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</Sum>
		<K>the dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K>
		<H>the proton concentration in a unit consistent with all othert input variables (e.g. mol/kg-solution)</H>
	</arguments>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</HAuni>

