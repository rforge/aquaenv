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

<dAdH_bi>
	<title>dAdH\_bi</title>
	<description>PRIVATE function: calculates the derivative of [A(2-)] of a bivalent acid with respect to [H+]</description>
	<usage>dAdH\_bi(H, SumA, K1, K2)</usage>
	<arguments>
		<H>the proton concentration in a unit consistent with all other input variables (e.g. mol/kg-solution)</H>
		<SumA>the total concentration of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</SumA>
		<K1>the first dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K1>
		<K2>the second dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K2>
	</arguments>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</dAdH_bi>

<dAdH_tri>
	<title>dAdH\_tri</title>
	<description>PRIVATE function: calculates the derivative of [A(3-)] of a trivalent acid with respect to [H+]</description>
	<usage>dAdH\_tri(H, SumA, K1, K2, K3)</usage>
	<arguments>
		<H>the proton concentration in a unit consistent with all other input variables (e.g. mol/kg-solution)</H>
		<SumA>the total concentration of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</SumA>
		<K1>the first dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K1>
		<K2>the second dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K2>
		<K3>the third dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K3>
	</arguments>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</dAdH_tri>

<dAdH_uni>
	<title>dAdH\_uni</title>
	<description>PRIVATE function: calculates the derivative of [A(-)] of a univalent acid with respect to [H+]</description>
	<usage>dAdH\_uni(H, SumA, K)</usage>
	<arguments>
		<H>the proton concentration in a unit consistent with all other input variables (e.g. mol/kg-solution)</H>
		<SumA>the total concentration of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</SumA>
		<K>the dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K>
	</arguments>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</dAdH_uni>

<dH2AdH_bi>
	<title>dH2AdH\_bi</title>
	<description>PRIVATE function: calculates the derivative of [H2A] of a bivalent acid with respect to [H+]</description>
	<usage>dH2AdH\_bi(H, SumA, K1, K2)</usage>
	<arguments>
		<H>the proton concentration in a unit consistent with all other input variables (e.g. mol/kg-solution)</H>
		<SumA>the total concentration of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</SumA>
		<K1>the first dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K1>
		<K2>the second dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K2>
	</arguments>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</dH2AdH_bi>

<dH2AdH_tri>
	<title>dH2AdH\_tri</title>
	<description>PRIVATE function: calculates the derivative of [H2A(-)] of a trivalent acid with respect to [H+]</description>
	<usage>dH2AdH\_tri(H, SumA, K1, K2, K3)</usage>
	<arguments>
		<H>the proton concentration in a unit consistent with all other input variables (e.g. mol/kg-solution)</H>
		<SumA>the total concentration of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</SumA>
		<K1>the first dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K1>
		<K2>the second dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K2>
		<K3>the third dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K3>
	</arguments>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</dH2AdH_tri>

<dH3AdH_tri>
	<title>dH3AdH\_tri</title>
	<description>PRIVATE function: calculates the derivative of [H3A] of a trivalent acid with respect to [H+]</description>
	<usage>dH3AdH\_tri(H, SumA, K1, K2, K3)</usage>
	<arguments>
		<H>the proton concentration in a unit consistent with all other input variables (e.g. mol/kg-solution)</H>
		<SumA>the total concentration of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</SumA>
		<K1>the first dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K1>
		<K2>the second dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K2>
		<K3>the third dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K3>
	</arguments>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</dH3AdH_tri>

<dHAdH_bi>
	<title>dHAdH\_bi</title>
	<description>PRIVATE function: calculates the derivative of [HA(-)] of a bivalent acid with respect to [H+]</description>
	<usage>dHAdH\_bi(H, SumA, K1, K2)</usage>
	<arguments>
		<H>the proton concentration in a unit consistent with all other input variables (e.g. mol/kg-solution)</H>
		<SumA>the total concentration of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</SumA>
		<K1>the first dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K1>
		<K2>the second dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K2>
	</arguments>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</dHAdH_bi>

<dHAdH_tri>
	<title>dHAdH\_tri</title>
	<description>PRIVATE function: calculates the derivative of [HA(2-)] of a trivalent acid with respect to [H+]</description>
	<usage>dHAdH\_tri(H, SumA, K1, K2, K3)</usage>
	<arguments>
		<H>the proton concentration in a unit consistent with all othert input variables (e.g. mol/kg-solution)</H>
		<SumA>the total concentration of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</SumA>
		<K1>the first dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K1>
		<K2>the second dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K2>
		<K3>the third dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K3>
	</arguments>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</dHAdH_tri>

<dHAdH_uni>
	<title>dHAdH\_uni</title>
	<description>PRIVATE function: calculates the derivative of [HA] of a univalent acid with respect to [H+]</description>
	<usage>dHAdH\_uni(H, SumA, K)</usage>
	<arguments>
		<H>the proton concentration in a unit consistent with all other input variables (e.g. mol/kg-solution)</H>
		<SumA>the total concentration of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</SumA>
		<K>the dissociation constant of the acid in question in a unit consistent with all other input variables (e.g. mol/kg-solution)</K>
	</arguments>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</dHAdH_uni>

<dTAdH>
	<title>dTAdH</title>
	<description>PRIVATE function: calculates the derivative of [TA] with respect to [H+]: the buffer factor </description>
	<usage>dTAdH(ae)</usage>
	<arguments>
		<ae>object of class aquaenv</ae>
	</arguments>
	<value>
		<Default>derivative of [TA] with respect to [H+]: the buffer factor</Default>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</dTAdH>

<dTAdKdKdd>
	<title>dTAdKdKdd</title>
	<description>PRIVATE function: calculates the derivative of [TA] with respect to changes in the dissociation constants (Ks) times the derivative of the dissociation constants with respect to depth d</description>
	<usage>dTAdKdKdd(ae)</usage>
	<arguments>
		<ae>object of class aquaenv</ae>
	</arguments>
	<value>
		<Default>derivative of [TA] with respect to changes in the dissociation constants (Ks) times the derivative of the dissociation constants with respect to depth d</Default>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</dTAdKdKdd>

<dTAdKdKdS>
	<title>dTAdKdKdS</title>
	<description>PRIVATE function: calculates the derivative of [TA] with respect to changes in the dissociation constants (Ks) times the derivative of the dissociation constants with respect to salinity S</description>
	<usage>dTAdKdKdS(ae)</usage>
	<arguments>
		<ae>object of class aquaenv</ae>
	</arguments>
	<value>
		<Default>derivative of [TA] with respect to changes in the dissociation constants (Ks) times the derivative of the dissociation constants with respect to salinity S</Default>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</dTAdKdKdS>

<dTAdKdKdSumH2SO4>
	<title>dTAdKdKdSumH2SO4</title>
	<description>PRIVATE function: calculates the derivative of [TA] with respect to changes in the dissociation constants (Ks) times the derivative of the dissociation constants with respect to the total sulfate concentration (influence via scale conversion)</description>
	<usage>dTAdKdKdSumH2SO4(ae)</usage>
	<arguments>
		<ae>object of class aquaenv</ae>
	</arguments>
	<value>
		<Default>derivative of [TA] with respect to changes in the dissociation constants (Ks) times the derivative of the dissociation constants with respect to the total sulfate concentration (influence via scale conversion)</Default>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</dTAdKdKdSumH2SO4>

<dTAdKdKdSumHF>
	<title>dTAdKdKdSumHF</title>
	<description>PRIVATE function: calculates the derivative of [TA] with respect to changes in the dissociation constants (Ks) times the derivative of the dissociation constants with respect to the total fluoride concentration (influence via scale conversion)</description>
	<usage>dTAdKdKdSumHF(ae)</usage>
	<arguments>
		<ae>object of class aquaenv</ae>
	</arguments>
	<value>
		<Default>derivative of [TA] with respect to changes in the dissociation constants (Ks) times the derivative of the dissociation constants with respect to the total fluoride concentration (influence via scale conversion)</Default>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</dTAdKdKdSumHF>

<dTAdKdKdT>
	<title>dTAdKdKdT</title>
	<description>PRIVATE function: calculates the derivative of [TA] with respect to changes in the dissociation constants (Ks) times the derivative of the dissociation constants with respect to temperature T</description>
	<usage>dTAdKdKdT(ae)</usage>
	<arguments>
		<ae>object of class aquaenv</ae>
	</arguments>
	<value>
		<Default>derivative of [TA] with respect to changes in the dissociation constants (Ks) times the derivative of the dissociation constants with respect to temperature T</Default>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</dTAdKdKdT>

<revelle>
	<title>revelle</title>
	<description>PRIVATE function: calculates the revelle factor</description>
	<usage>revelle(ae)</usage>
	<arguments>
		<ae>object of class aquaenv</ae>
	</arguments>
	<value>
		<Default>the revelle factor</Default>
	</value>
	<references>Sundquist1979, Zeebe2001, Emerson2008</references>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</revelle>

