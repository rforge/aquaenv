###################################################
# operations on objects of type AQUAENV
###################################################


# PRIVATE function:
# converts all elements of a special unit or pH scale in an object of class aquaenv                   
convert.aquaenv <- function(aquaenv,           # object of class aquaenv 
                            from,              # the unit which needs to be converted (as a string; must be a perfect match)
                            to,                # the unit to which the conversion should go
                            factor,            # the conversion factor to be applied: can either be a number (e.g. 1000 to convert from mol to mmol), or any of the conversion factors given in an object of class  aquaenv 
                            convattr="unit",   # which attribute should be converted? can either be "unit" or "pH scale"
                            ...) 
  {
    for (x in names(aquaenv))
      {
        if (!is.null(attr(aquaenv[[x]], convattr)))
            {
              if (attr(aquaenv[[x]], convattr) == from)
                {
                  aquaenv[[x]]                 <- aquaenv[[x]] * factor
                  attr(aquaenv[[x]], convattr) <- to
                }
            }
      }
    return(aquaenv)                            # object of class aquaenv whith the converted elements
  }


# PRIVATE function:
# returns the (maximal) length of the elements in an object of class aquaenv (i.e. > 1 if one of the input variables was a vector)
length.aquaenv <- function(x,           # object of class aquaenv
                           ...)
  {
    for (e in x)
      {
        if (length(e) > 1)
          {
            return(length(e))                 # the maximal length of the elements in the object of class aquaenv
          }
      }
    return(1)                                 # the maximal length of the elements in the object of class aquaenv
  }


# PRIVATE function:
# adds an element to an object of class aquaenv
c.aquaenv <- function(aquaenv,                # object of class aquaenv
                      x,                      # a vector of the form c(value, name) representing the element to be inserted into the object of class aquaenv
                      ...)
  {
    aquaenv[[x[[2]]]] <- x[[1]]
    return(aquaenv)                           # object of class aquaenv with the added element
  }


# PRIVATE function:
# merges the elements of two objects of class aquaenv: element names are taken from the first argument, the elements of which are also first in the merged object
merge.aquaenv <- function(x,           # object of class aquaenv: this is where the element names are taken from
                          y,           # object of class aquaenv: must contain at leas all the element (names) as x, extra elements are ignored
                          ...)
  {
    nam <- names(x)
    for (n in nam)
      {
        unit <- attr(x[[n]], "unit")
        x[[n]] <- c(x[[n]], y[[n]])
        attr(x[[n]], "unit") <- unit
      }
    return(x)                          # object of class aquaenv with merged elements
  }


# PRIVATE function:
# clones an object of class aquaenv: it is possible to supply a new value for either TA or pH; the switches speciation, skeleton, revelle, and dsa are obtained from the object to be cloned
cloneaquaenv <- function(aquaenv,             # object of class aquaenv
                         TA=NULL,             # optional new value for TA
                         pH=NULL)             # optional new value for pH
  {
    if (is.null(TA) && is.null(pH))
      {
        pH <- aquaenv$pH
      }
    return(                                   # cloned object of class aquaenv
           aquaenv(Tc=aquaenv$Tc, S=aquaenv$S, d=aquaenv$d, SumCO2=aquaenv$SumCO2, SumNH4=aquaenv$SumNH4, SumH2S=aquaenv$SumH2S,SumH3PO4=aquaenv$SumH3PO4,
                   SumSiOH4=aquaenv$SumSiOH4, SumHNO3=aquaenv$SumHNO3, SumHNO2=aquaenv$SumHNO2, SumBOH3=aquaenv$SumBOH3, SumH2SO4=aquaenv$SumH2SO4,
                   SumHF=aquaenv$SumHF, pH=pH, TA=TA,
                   speciation=(!is.null(aquaenv$HCO3)), skeleton=(is.null(aquaenv$Na)), revelle=(!is.null(aquaenv$revelle)), dsa=(!is.null(aquaenv$dTAdH))))
  }


# PRIVATE function:
# creates an object of class aquaenv from a data frame (e.g. as supplied from the numerical solver of a dynamic model)
from.data.frame <- function(df)               # data frame
  {
    temp        <- as.list(df)
    class(temp) <- "aquaenv"
    return(temp)                              # object of class aquaenv
  }





#########################################################
# CONVERSION functions
#########################################################


# PRIVATE function:
# converts either the pH scale of a pH value, the pH scale of a dissociation constant (K*), or the unit of a concentration value
convert.standard <- function(x,               # the object to be converted (pH value, K* value, or concentration value)
                             vartype,         # the type of x, either "pHscale", "KHscale", or "conc"
                             what,            # the type of conversion to be done, for pH scales one of "free2tot", "free2sws", "free2nbs", ... (any combination of "free", "tot", "sws", and "nbs"); for concentrations one of "molar2molal", "molar2molin", ... (any combination of "molar" (mol/l), "molal" (mol/kg-H2O), and "molin" (mol/kg-solution))
                             Tc,              # temperature in degrees centigrade
                             S,               # salinity (in practical salinity units: no unit)
                             d=0,             # depth in meters
                             SumH2SO4=NULL,   # total sulfate concentration in mol/kg-solution; if not supplied this is calculated from S
                             SumHF=NULL)      # total fluoride concentration in mol/kg-solution; if not supplied this is calculated from S
  {
    result <- (switch
               (vartype,
                pHscale = switch
                (what,
                 free2tot = x - log10(scaleconvert(Tc, S, d, SumH2SO4, SumHF)$free2tot),
                 free2sws = x - log10(scaleconvert(Tc, S, d, SumH2SO4, SumHF)$free2sws),
                 free2nbs = x - log10(scaleconvert(Tc, S, d, SumH2SO4, SumHF)$free2nbs),
                 tot2free = x - log10(scaleconvert(Tc, S, d, SumH2SO4, SumHF)$tot2free),
                 tot2sws  = x - log10(scaleconvert(Tc, S, d, SumH2SO4, SumHF)$tot2sws),
                 tot2nbs  = x - log10(scaleconvert(Tc, S, d, SumH2SO4, SumHF)$tot2nbs),
                 sws2free = x - log10(scaleconvert(Tc, S, d, SumH2SO4, SumHF)$sws2free),
                 sws2tot  = x - log10(scaleconvert(Tc, S, d, SumH2SO4, SumHF)$sws2tot),
                 sws2nbs  = x - log10(scaleconvert(Tc, S, d, SumH2SO4, SumHF)$sws2nbs),
                 nbs2free = x - log10(scaleconvert(Tc, S, d, SumH2SO4, SumHF)$nbs2free),
                 nbs2tot  = x - log10(scaleconvert(Tc, S, d, SumH2SO4, SumHF)$nbs2tot),
                 nbs2sws  = x - log10(scaleconvert(Tc, S, d, SumH2SO4, SumHF)$nbs2sws)
                 ),
                KHscale = switch
                (what,
                 free2tot = x * scaleconvert(Tc, S, d, SumH2SO4, SumHF)$free2tot,
                 free2sws = x * scaleconvert(Tc, S, d, SumH2SO4, SumHF)$free2sws,
                 free2nbs = x * scaleconvert(Tc, S, d, SumH2SO4, SumHF)$free2nbs,
                 tot2free = x * scaleconvert(Tc, S, d, SumH2SO4, SumHF)$tot2free,
                 tot2sws  = x * scaleconvert(Tc, S, d, SumH2SO4, SumHF)$tot2sws,
                 tot2nbs  = x * scaleconvert(Tc, S, d, SumH2SO4, SumHF)$tot2nbs,
                 sws2free = x * scaleconvert(Tc, S, d, SumH2SO4, SumHF)$sws2free,
                 sws2tot  = x * scaleconvert(Tc, S, d, SumH2SO4, SumHF)$sws2tot,
                 sws2nbs  = x * scaleconvert(Tc, S, d, SumH2SO4, SumHF)$sws2nbs,
                 nbs2free = x * scaleconvert(Tc, S, d, SumH2SO4, SumHF)$nbs2free,
                 nbs2tot  = x * scaleconvert(Tc, S, d, SumH2SO4, SumHF)$nbs2tot,
                 nbs2sws  = x * scaleconvert(Tc, S, d, SumH2SO4, SumHF)$nbs2sws
                 ),
                conc = switch
                (what,
                 molar2molal = x * (1/((seadensity(Tc,S)/1e3)* molal2molin(S))),
                 molar2molin = x * (1/((seadensity(Tc,S))/1e3))                ,
                 molal2molar = x * (molal2molin(S) * (seadensity(Tc,S))/1e3)   ,
                 molal2molin = x * (molal2molin(S))                            ,
                 molin2molar = x * (seadensity(Tc,S)/1e3)                      ,
                 molin2molal = x * (1/molal2molin(S))
                 )
                )
               )
    if ((what == "tot2free") || (what == "sws2free") || (what == "nbs2free"))
      {
        attr(result, "pH scale") <- "free"
      }
    else if ((what == "free2tot") || (what == "sws2tot") || (what == "nbs2tot"))
      {
        attr(result, "pH scale") <- "tot"
      }
    else if ((what == "free2nbs") || (what == "sws2nbs") || (what == "tot2nbs"))
      {
        attr(result, "pH scale") <- "nbs"
      }
    else if ((what == "molar2molal") || (what == "molin2molal"))
      {
        attr(result, "unit") <- "mol/kg-H2O"
      }
    else if ((what == "molal2molin") || (what == "molar2molin"))
      {
        attr(result, "unit") <- "mol/kg-soln"
      }
    else if ((what == "molal2molar") || (what == "molin2molar"))
      {
        attr(result, "unit") <- "mol/l-soln"
      }
    return(result)                            # converted pH, K*, or concentration value, attributed with the new unit/pH scale
  }


# PRIVATE function:
# calculates the hydrostatic pressure from the depth (the hydrostatic pressure increases per m depth by 1/10 of 1 atm)
hydroP <- function(d)                         # depth in meters
  {
    return(0.1*d*1.01326)                     # hydrostatic pressure in bars
  }


# PRIVATE function:
# calculates the ionic strength I as a function of salinity S
# references: DOE1994, Zeebe2001, Roy1993b (the carbonic acid paper)
I <- function(S)                              # salinity S in practical salinity units (i.e. no unit)
  {
    return(19.924*S/(1000-1.005*S))           # ionic strength in mol/kg-solution (molinity)
  }


# PRIVATE function:
# calculates chlorinity Cl from salinity S
# references: DOE1994, Zeebe2001
Cl <- function(S)                             # salinity S in practical salinity units (i.e. no unit)
  {
    return(S/1.80655)                         # chlorinity Cl in permil
  }


# PRIVATE function:
# calculates concentrations of constituents of natural seawater from a given salinity S
# reference: DOE1994
seaconc <- function(spec,                     # constituent of seawater (chemical species) of which the concentration should be calculated. can be any name of the vectors ConcRelCl and MeanMolecularWeight: "Cl", "SO4", "Br", "F", "Na", "Mg", "Ca", "K", "Sr", "B", "S"
                    S)                        # salinity S in practical salinity units (i.e. no unit)
  {
    return(                                   # concentration of the constituent of seawater speciefied in spec in mol/kg-solution (molinity): this is determined by the data in ConcRelCl and MeanMolecularWeight
           ConcRelCl[[spec]]/MeanMolecularWeight[[spec]]*Cl(S))   
  }
                    

# PRIVATE function:
# calculates the conversion factor converting from molality (mol/kg-H2O) to molinity (mol/kg-solution) from salinity S
# reference: Roy1993b (the carbonic acid paper), DOE1994
molal2molin <- function(S)                    # salinity S in practical salinity units (i.e. no unit)  
  {
    return(1-0.001005*S)                      # the conversion factor from molality (mol/kg-H2O) to molinity (mol/kg-solution)
  }


# PRIVATE function:
# calculates the temperature in Kelvin from the temperature in degrees centigrade
Tk <- function(Tc)                            # temperature in degrees centigrade
  {
    return(Tc - Constants$absZero)            # temperature in Kelvin
  }


# PRIVATE function:
# provides pH scale conversion factors (caution: the activity coefficient for H+ (needed for NBS scale conversions) is calculated with the Davies equation (Zeebe2001) which is only accurate up to ionic strengthes of I = 0.5)
# references: Dickson1984, DOE1994, Zeebe2001
scaleconvert <- function(Tc,                   # temperature in degrees centigrade
                         S,                    # salinity S in practical salinity units (i.e. no unit)  
                         d=0,                  # depth in meters
                         SumH2SO4=NULL,        # total sulfate concentration in mol/kg-solution; if not supplied this is calculated from S
                         SumHF=NULL)           # total fluoride concentration in mol/kg-solution; if not supplied this is calculated from S
  {
    if (is.null(SumH2SO4))
      {
        SumH2SO4 = seaconc("SO4", S)
      }
    if (is.null(SumHF))
      {
        SumHF = seaconc("F", S)
      }

    K_HSO4 <- K_HSO4(Tc, S, d)
    K_HF   <- K_HF(Tc, S, d)
    
    FreeToTot <- (1 + (SumH2SO4/K_HSO4))
    FreeToSWS <- (1 + (SumH2SO4/K_HSO4) + (SumHF/K_HF))
    attributes(FreeToTot) <- NULL
    attributes(FreeToSWS) <- NULL

    SQRTI     <- sqrt(I(S))
    eTk       <- Constants$e*Tk(Tc)
    A         <- 1.82e6/(eTk*sqrt(eTk))
    NBSToFree <- 10^(A*((SQRTI/(1+SQRTI)) - 0.2*I(S))) #davies equation: only valid up to I=0.5
 
    return(list(                              # list of conversion factors "free2tot", "free2sws", etc.
                free2tot = FreeToTot,
                free2sws = FreeToSWS,
                free2nbs = 1/NBSToFree,
                tot2free = 1/FreeToTot,
                tot2sws  = 1/FreeToTot * FreeToSWS,
                tot2nbs  = 1/FreeToTot * 1/NBSToFree,
                sws2free = 1/FreeToSWS,
                sws2tot  = 1/FreeToSWS * FreeToTot,
                sws2nbs  = 1/FreeToSWS * 1/NBSToFree,
                nbs2free = NBSToFree,
                nbs2tot  = NBSToFree * FreeToTot,
                nbs2sws  = NBSToFree * FreeToSWS
                ))
  }


# PRIVATE function:
# calculates seawater density (in kg/m3) from temperature (in degrees centigrade) and salinity
# references: Millero1981, DOE1994
seadensity <- function(Tc,                    # temperature in degrees centigrade
                       S)                     # salinity S in practical salinity units (i.e. no unit)  
  {
    Tc2 <- T^2
    Tc3 <- Tc2 * Tc
    Tc4 <- Tc3 * Tc
    Tc5 <- Tc4 * Tc
            
    A <- 0.824493    - 4.0899e-3*Tc + 7.6438e-5*Tc2 - 8.2467e-7*Tc3 + 5.3875e-9*Tc4
    B <- -5.72466e-3 + 1.0227e-4*Tc - 1.6546e-6*Tc2
    C <- 4.8314e-4

    densityWater <- 999.842594 + 6.793952e-2*Tc - 9.095290e-3*Tc2 + 1.001685e-4*Tc3 - 1.120083e-6*Tc4 + 6.536332e-9*Tc5
        
    return(                                  # seawater density in kg/m3
           densityWater + A*S + B*S*sqrt(S) + C*S^2) 
  }





################################################################
# input / output (IO) functions
################################################################


# PRIVATE function:
# basic wrapper for the R plot function for plotting objects of class aquaenv; no return value, just side-effect
basicplot <- function(aquaenv,                # object of class aquaenv
                      xval,                   # x-value: the independent variable describing a change in elements of an object of class aquaenv
                      type="l",               # standard plot parameter;     default: plot lines
                      mgp=c(1.8, 0.5, 0),     # standard plot parameter;     default: axis title on line 1.8, axis labels on line 0.5, axis on line 0
                      mar=c(3,3,0.5,0.5),     # standard plot parameter;     default: margin of 3 lines bottom and left and 0.5 lines top and right
                      oma=c(0,0,0,0),         # standard plot parameter;     default: no outer margin
                      size=c(15,13),          # the size of the plot device; default: 15 (width) by 13 (height) inches
                      mfrow=c(11,10),         # standard plot parameter;     default: 11 columns and 10 rows of plots
                      device="x11",           # the device to plot on;       default: "x11" (can also be "eps" or "pdf")
                      filename="aquaenv",     # filename to be used if "eps" or "pdf" is selected for device
                      ...)
  {
    opendevice(device, size, filename)
    par(mfrow=mfrow, mar=mar, oma=oma, mgp=mgp)
    aquaenv <- as.data.frame(aquaenv)
    for (i in 1:length(aquaenv))
      {
        plot(xval, aquaenv[[i]], ylab=names(aquaenv)[[i]], type=type,  ...)
      }
  } 


# PRIVATE function:
# opens a device for plotting; no return value, just side-effect 
opendevice <- function(device,                 # either "x11", "eps", or "pdf"
                       size,                   # size of the plot device in the form c(width, height)
                       filename)               # filename to use if "eps" or "pdf" is used
  {
    if (device == "x11")
      {
        x11(width=size[[1]], height=size[[2]])
      }
    else if (device == "eps")
      {
        postscript(width=size[[1]], height=size[[2]], file=paste(filename, ".eps", sep=""), paper="special")
      }
    else if (device == "pdf")
      {
        pdf(width=size[[1]], height=size[[2]], file=paste(filename, ".pdf", sep=""), paper="special")
      }
  }


# PRIVATE function:
# plots all elements of an object of class aquaenv; no return value, just side-effect 
plotall <- function(aquaenv,                  # object of class aquaenv
                    xval,                     # x-value: the independent variable describing a change in elements of an object of class aquaenv
                    ...)
  {
    basicplot(aquaenv, xval=xval, ...)
  }


# PRIVATE function:
# plots just the elements of an object of class aquaenv given in what; no return value, just side-effect 
selectplot <- function(aquaenv,               # object of class aquaenv
                       xval,                  # x-value: the independent variable describing a change in elements of an object of class aquaenv
                       what,                  # vector of names of elements of aquaenv that should be plotted
                       mfrow=c(1,1),          # standard plot parameter; default: just one plot
                       size=c(7,7),           # the size of the plot device; default: 7 (width) by 7 (height) inches
                       ...)
  {
    aquaenvnew <- aquaenv[what]
    class(aquaenvnew) <- "aquaenv"
    basicplot(aquaenvnew, xval=xval, mfrow=mfrow, size=size, ...)
  }


# PRIVATE function:
# creates a bjerrumplot from the elements of an object of class aquaenv given in what; no return value, just side-effect 
bjerrumplot <- function(aquaenv,              # object of class aquaenv
                        what,                 # vector of names of elements of aquaenv that should be plotted; if not specified:  what <- c("CO2", "HCO3", "CO3", "BOH3", "BOH4", "OH", "H3PO4", "H2PO4", "HPO4", "PO4", "SiOH4", "SiOOH3", "SiO2OH2", "H2S", "HS", "S2min", "NH4", "NH3", "H2SO4", "HSO4", "SO4", "HF", "F", "HNO3", "NO3", "HNO2", "NO2")
                        log=FALSE,            # should the plot be on a logarithmic y axis? 
                        palette=NULL,         # a vector of colors to use in the plot (either numbers or names given in colors())
                        device="x11",         # the device to plot on; default: "x11" (can also be "eps" or "pdf")
                        filename="aquaenv",   # filename to be used if "eps" or "pdf" is selected for device
                        size=c(12,10),        # the size of the plot device; default: 12 (width) by 10 (height) inches
                        ylim=NULL,            # standard plot parameter; if not supplied it will be calculated by range() of the elements to plot
                        lwd=2,                # standard plot parameter; width of the lines in the plot
                        xlab="free scale pH", # x axis label
                        mgp=c(1.8, 0.5, 0),   # standard plot parameter; default: axis title on line 1.8, axis labels on line 0.5, axis on line 0
                        mar=c(3,3,0.5,0.5),   # standard plot parameter; default: margin of 3 lines bottom and left and 0.5 lines top and right
                        oma=c(0,0,0,0),       # standard plot parameter; default: no outer margin
                        legendposition="bottomleft", # position of the legend
                        legendinset=0.05,     # standard legend parameter inset   
                        legendlwd=4,          # standard legend parameter lwd: line width of lines in legend
                        bg="white",           # standard legend parameter: default background color: white
                        ...)
  {
    if (is.null(what))
      {
        what <- c("CO2", "HCO3", "CO3", "BOH3", "BOH4", "OH", "H3PO4", "H2PO4", "HPO4", "PO4", "SiOH4", "SiOOH3", "SiO2OH2",
                   "H2S", "HS", "S2min", "NH4", "NH3", "H2SO4", "HSO4", "SO4", "HF", "F", "HNO3", "NO3", "HNO2", "NO2")
      }
    bjerrumvarslist <- aquaenv[what]
    class(bjerrumvarslist) <- "aquaenv"
    bjerrumvars <- as.data.frame(bjerrumvarslist)
    
    opendevice(device, size, filename)
    par(mar=mar, mgp=mgp, oma=oma)

    if (is.null(palette))
      {
        palette <- 1:length(what)
      }

    if (log)
      {
        if (is.null(ylim))
          {
            ylim <- range(log10(bjerrumvars))
          }
        yvals <- log10(bjerrumvars)
        ylab  <- paste("log10([X]/(",attr(bjerrumvarslist[[1]], "unit"),"))", sep="")
      }
    else
      {
        if (is.null(ylim))
          {
            ylim  <- range(bjerrumvars)
          }
        yvals <- bjerrumvars
        ylab  <- attr(bjerrumvarslist[[1]], "unit")
      }
    for (i in 1:length(bjerrumvars))
      {
        plot(aquaenv$pH, yvals[[i]], type="l", ylab=ylab, xlab=xlab, ylim=ylim, col=palette[[i]], lwd=lwd, ...)
        par(new=TRUE)
      }
    par(new=FALSE)
    
    legend(legendposition, inset=legendinset, legend=names(bjerrumvarslist), col=palette, bg=bg, lwd=legendlwd, ...)
  }


# PRIVATE function:
# creates a cumulative plot from the elements of an object of class aquaenv given in what; no return value, just side-effect 
cumulativeplot <- function(aquaenv,           # object of class aquaenv
                           xval,              # x-value: the independent variable describing a change in elements of an object of class aquaenv
                           what,              # vector of names of elements of aquaenv that should be plotted
                           total=TRUE,        # should the sum of all elements specified in what be plotted as well?
                           palette=NULL,      # a vector of colors to use in the plot (either numbers or names given in colors())
                           device="x11",      # the device to plot on;       default: "x11" (can also be "eps" or "pdf")
                           filename="aquaenv",# filename to be used if "eps" or "pdf" is selected for device
                           size=c(12,10),     # the size of the plot device; default: 12 (width) by 10 (height) inches
                           ylim=NULL,         # standard plot parameter; if not supplied it will be calculated by an adaptation of range() of the elements to plot
                           lwd=2,             # standard plot parameter; width of the lines in the plot
                           mgp=c(1.8, 0.5, 0),# standard plot parameter; default: axis title on line 1.8, axis labels on line 0.5, axis on line 0
                           mar=c(3,3,0.5,0.5),# standard plot parameter; default: margin of 3 lines bottom and left and 0.5 lines top and right
                           oma=c(0,0,0,0),    # standard plot parameter; default: no outer margin
                           legendposition="bottomleft", # position of the legend
                           legendinset=0.05,  # standard legend parameter inset  
                           legendlwd=4,       # standard legend parameter lwd: line width of lines in legend
                           bg="white",        # standard legend parameter: default background color: white
                           y.intersp=1.2,     # standard legend parameter; default: 1.2 lines space between the lines in the legend
                           ...)
  {
    if (is.null(what))
      {
        what=names(aquaenv)
      }

    cumulativevarslist <- aquaenv[what]
    class(cumulativevarslist) <- "aquaenv"
    cumulativevars <- as.data.frame(cumulativevarslist)
    
    if (is.null(ylim))
      {
        ylim <- c(0,0)
        for (var in cumulativevars)
          {
            ylim <- ylim + range(c(var,0))
          }
      }
    if (is.null(palette))
      {
        palette <- 1:length(names(cumulativevars))
      }
    
    opendevice(device, size, filename)
    par(mar=mar, mgp=mgp, oma=oma)
        
    plot(xval, rep(0,length(xval)), type="l", ylim=ylim, col="white", ...)
    sumfuncpos <- rep(0,length(xval))
    for (x in 1:(length(cumulativevars)))
      {
        yval <- (cumulativevars[[x]]) 
        yval[yval<=0] <- 0
        newsumfuncpos <- sumfuncpos + yval

        if (!(identical(yval, rep(0,length(yval)))))
          {
            polygon(c(xval,rev(xval)), c(newsumfuncpos, rev(sumfuncpos)), col=palette[[x]], border=NA)
          }
        
        sumfuncpos <- newsumfuncpos
      }
    sumfuncneg <- rep(0,length(xval))
    for (x in 1:(length(cumulativevars)))
      {
        yval <- (cumulativevars[[x]])
        yval[yval>=0] <- 0
        newsumfuncneg <- sumfuncneg + yval
        
        if (!(identical(yval, rep(0,length(yval)))))
          {
            polygon(c(xval,rev(xval)), c(newsumfuncneg, rev(sumfuncneg)), col=palette[[x]], border=NA)
          }
        
        sumfuncneg <- newsumfuncneg
      }
      if (total)
      {
        par(new=TRUE)
        plot(xval, apply(cumulativevars, 1, sum), col="gray", type="l", ylim=ylim, xlab="", ylab="", lwd=lwd)
      }	
    legend(legendposition, legend=names(cumulativevars), col=palette, inset=legendinset, y.intersp=y.intersp, bg=bg, lwd=legendlwd)    
  }

