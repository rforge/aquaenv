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

<TAfit>
	<title>TAfit</title>
	<description>PUBLIC function: calculates [TA] and [SumCO2] from a titration curve using an optimization procedure (nls.lm from R package minpack.lm)</description>
	<usage>TAfit(ae, pHmeasurements, volume, amount, TAguess=0.0025, type="HCl")</usage>
	<arguments>
		<ae>an object of type aquaenv: minimal definition, contains all information about the system: T, S, d, total concentrations of nutrients etc</ae>
		<pHmeasurements>a table containing the titration curve: basically a series of pH values (pH on free proton scale)</pHmeasurements>
		<volume>the volume of the titration vessel</volume>
		<amount>the total amount of the titrant added</amount>
		<TAguess>a first guess for [TA] and [SumCO2] to be used as initial values for the optimization procedure</TAguess>
		<type>the type of titrant: either "HCl" or "NaOH"</type>
	</arguments>
	<value>
		<Default>a list of three values ([TA] in mol/kg-solution, [SumCO2] in mol/kg-solution, sum of the squared residuals)</Default>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</TAfit>

<titration>
	<title>titration</title>
	<description>PUBLIC function: creates an object of class aquaenv which contains a titration simulation (changes in [Na] and [Cl] and therefore in S are assumed negligible (~ 5 mmol/kg wrt 500 mmol/kg))</description>
	<usage>titration(aquaenv, volume, amount, steps, type)</usage>
	<arguments>
		<aquaenv>an object of type aquaenv: minimal definition, contains all information about the system: T, S, d, total concentrations of nutrients etc</aquaenv>
		<volume>the volume of the (theoretical) titration vessel in l</volume>
		<amount>the amount of titrant added in mol</amount>
		<steps>the amount of steps the amount of titrant is added in</steps>
		<type>the type of titrant: either "HCl" or "NaOH"</type>
	</arguments>
	<value>
		<Default>object of class aquaenv which contains a titration simulation</Default>
	</value>
	<author>Andreas F. Hofmann</author>
	<keyword>misc</keyword>
</titration>

