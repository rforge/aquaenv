\name{plot.aquaenv}
\alias{plot.aquaenv}
\title{plot.aquaenv}
\description{PUBLIC function: high level plot function for objects of  class aquaenv}
\details{Top level generic usage is
  \preformatted{plot.aquaenv(x, xval, what=NULL, bjerrum=FALSE,
             cumulative=FALSE, newdevice=TRUE, setpar=TRUE,
	     device="x11", ...)}
  Generic usages for standard plotting are
  \preformatted{plot.aquaenv(x, xval, ...)}
  \preformatted{plot.aquaenv(x, xval, what, mfrow=c(1,1), size=c(7,7), ...)}
  Generic usage for creating a bjerrum plot is
  \preformatted{plot.aquaenv(x, what, log=FALSE, palette=NULL,
             device="x11", filename="aquaenv",
             size=c(12,10), ylim=NULL, lwd=2, xlab="free scale pH",
	     mgp=c(1.8, 0.5, 0), mar=c(3,3,0.5,0.5), oma=c(0,0,0,0),
	     legendposition="bottomleft", legendinset=0.05,
	     legendlwd=4, bg="white", newdevice=TRUE, setpar=TRUE,
	     device="x11",...)}
  Generic usage for creating a cumulative plot is
  \preformatted{plot.aquaenv(x, xval, what, total=TRUE, palette=NULL,
             device="x11", filename="aquaenv", size=c(12,10), ylim=NULL,
             lwd=2, mgp=c(1.8, 0.5, 0), mar=c(3,3,0.5,0.5), oma=c(0,0,0,0),
             legendposition="bottomleft", legendinset=0.05, legendlwd=4,
             bg="white", y.intersp=1.2, newdevice=TRUE, setpar=TRUE,
	     device="x11",...)}
}
\arguments{
  \item{x}{object of class aquaenv}
  \item{xval }{only valid if bjerrum=FALSE: a vector of the (maximal) length of the elements of aquaenv against which they are to be plotted}
  \item{what }{a list of names of the elements of aquaenv that are to be plotted, if not supplied and bjerrum=FALSE and cumulative=FALSE: all elements are plotted, if not supplied and bjerrum=TRUE then what is set to be c("CO2", "HCO3", "CO3", "BOH3", "BOH4", "OH", "H3PO4", "H2PO4", "HPO4", "PO4", "SiOH4", "SiOOH3", "SiO2OH2", "H2S", "HS", "S2min", "NH4", "NH3", "H2SO4", "HSO4", "SO4", "HF", "F", "HNO3", "NO3", "HNO2", "NO2"), needs to be supplied for cumulative=TRUE}
  \item{bjerrum }{flag: TRUE = a bjerrum plot is done (by calling bjerrumplot)}
  \item{cumulative }{flag: TRUE = a cumulative plot is done (by calling cumulativeplot)}
  \item{newdevice}{flag: if TRUE, new plot device is opened}
  \item{setpar}{flag: if TRUE parameters are set with the function par}
  \item{xlab }{x axis label}
  \item{log }{only valif if bjerrum=TRUE: should the plot be on a logarithmic y axis?}
  \item{total }{only valid if cumulative=TRUE: should the sum of all elements specified in what be plotted as well?}
  \item{device }{the device to plot on; default: "x11" (can also be "eps" or "pdf")}
  \item{filename }{filename to be used if "eps" or "pdf" is selected for device}
  \item{size }{the size of the plot device; default: 12 (width) by 10 (height) inches}
  \item{ylim }{standard plot parameter; if not supplied it will be calculated by range() of the elements to plot}
  \item{lwd }{standard plot parameter; width of the lines in the plot}
   \item{mgp }{standard plot parameter; default: axis title on line 1.8, axis labels on line 0.5, axis on line 0}
  \item{mar }{standard plot parameter; default: margin of 3 lines bottom and left and 0.5 lines top and right}
  \item{oma }{standard plot parameter; default: no outer margin}
  \item{palette }{only valid if bjerrum=TRUE or cumulative=TRUE: a vector of colors to use in the plot (either numbers or names given in colors())}
  \item{legendposition }{only valid if bjerrum=TRUE or cumulative=TRUE: position of the legend}
  \item{legendinset }{only valid if bjerrum=TRUE or cumulative=TRUE: standard legend parameter inset}
  \item{legendlwd }{only valid if bjerrum=TRUE or cumulative=TRUE: standard legend parameter lwd: line width of lines in legend}
  \item{bg }{only valid if bjerrum=TRUE or cumulative=TRUE: standard legend parameter: default background color: white}
  \item{y.intersp }{standard legend parameter; if cumulative=TRUE then default: 1.2 lines space between the lines in the legend}
  \item{... }{further arguments are passed on to the plot function}	
}
\examples{\dontrun{
### 1
#####

SumCO2 <- 0.0020
pH     <- 8

S      <- 30
t      <- 1:15
p      <- 10
ae <- aquaenv(S, t, p, SumCO2=SumCO2, pH=pH, revelle=TRUE, dsa=TRUE)
plot(ae, xval=t, xlab="T/(deg C)", newdevice=FALSE)



### 2
#####
S <- 35
t <- 15

SumCO2 <- 0.003500
SumNH4 <- 0.000020

mass_sample  <- 0.01 # the mass of the sample solution in kg
mass_titrant <- 0.02 # the total mass of the added titrant solution in
                     # kg
conc_titrant <- 0.01 # the concentration of the titrant solution in
                     # mol/kg-soln
S_titrant    <- 0.5  # the salinity of the titrant solution (the
                     # salinity of a solution with a ionic strength of
                     # 0.01 according to: I = (19.924 S) / (1000 - 1.005S)
steps        <- 50   # the amount of steps the mass of titrant is added
                     # in
type         <- "HCl"

pHstart <- 11.3


ae <- titration(aquaenv(S=S, t=t, SumCO2=SumCO2, SumNH4=SumNH4,
                pH=pHstart), mass_sample, mass_titrant, conc_titrant,
                S_titrant, steps, type)


# plotting everything
plot(ae, xval=ae$delta_mass_titrant, xlab="HCl solution added [kg]",
mfrow=c(10,10))


# plotting selectively
size  <- c(12,8) #inches
mfrow <- c(4,4)
what  <- c("TA", "pH", "CO2", "HCO3", "CO3", "BOH3", "BOH4", "OH",
           "NH4", "NH3", "H2SO4", "HSO4", "SO4", "HF", "F", "pCO2")


plot(ae, xval=ae$delta_mass_titrant, xlab="HCl solution added [kg]",
     what=what, size=size, mfrow=mfrow)

plot(ae, xval=ae$pH, xlab="free scale pH", what=what, size=size,
     mfrow=mfrow)


# different x values
plot(ae, xval=ae$delta_conc_titrant, xlab="[HCl] offset added
     [mol/kg-soln]", what=what, size=size, mfrow=mfrow)

plot(ae, xval=ae$delta_moles_titrant, xlab="HCl added [mol]", what=what,
     size=size, mfrow=mfrow, newdevice=FALSE)


# bjerrum plots
plot(ae, bjerrum=TRUE)

what  <- c("CO2", "HCO3", "CO3")
plot(ae, what=what, bjerrum=TRUE)
plot(ae, what=what, bjerrum=TRUE, lwd=4, palette=c("cyan", "magenta",
     "yellow"), bg="gray", legendinset=0.1, legendposition="topleft")



what  <- c("CO2", "HCO3", "CO3", "BOH3", "BOH4", "OH", "NH4", "NH3",
           "H2SO4", "HSO4", "SO4", "HF", "F")

plot(ae, what=what, bjerrum=TRUE, log=TRUE, newdevice=FALSE)
plot(ae, what=what, bjerrum=TRUE, log=TRUE, ylim=c(-6,-1),
     legendinset=0, lwd=3, palette=c(1,3,4,5,6,colors()[seq(100,250,6)]))


### 3
#####
parameters <- list(             
    t           = 15        , # degrees C
    S           = 35        , # psu       

    SumCO2_t0   = 0.002     , # mol/kg-soln  (comparable to Wang2005)
    TA_t0       = 0.0022    , # mol/kg-soln  (comparable to Millero1998)

    kc          = 0.5       , # 1/d	         proportionality factor
                              #                  for air-water exchange
    kp          = 0.000001  , # mol/(kg-soln*d)	 max rate of calcium
                              #                  carbonate precipitation
    n           = 2.0       , # -                exponent for kinetic
                              #                  rate law of precipitation
 
    modeltime   = 20        , # d              duration of the model
    outputsteps = 100         #                number of outputsteps
                   )

boxmodel <- function(timestep, currentstate, parameters)
{
  with (
        as.list(c(currentstate,parameters)),
        {        
          ae    <- aquaenv(S=S, t=t, SumCO2=SumCO2, pH=-log10(H), SumSiOH4=0, 
                           SumBOH3=0, SumH2SO4=0, SumHF=0, dsa=TRUE)
                   
          Rc    <- kc * ((ae$CO2_sat) - (ae$CO2)) 
          Rp    <- kp * (1-ae$omega_calcite)^n               

          dSumCO2 <- Rc - Rp

          dHRc    <- (      -(ae$dTAdSumCO2*Rc   ))/ae$dTAdH
          dHRp    <- (-2*Rp -(ae$dTAdSumCO2*(-Rp)))/ae$dTAdH
          dH      <- dHRc + dHRp
          
          ratesofchanges <- c(dSumCO2, dH)
          
          processrates   <- c(Rc=Rc, Rp=Rp)
          outputvars     <- c(dHRc=dHRc, dHRp=dHRp)
          
          return(list(ratesofchanges, list(processrates, outputvars, ae)))
        }
        )
}

with (as.list(parameters),
      {
        aetmp <- aquaenv(S=S, t=t, SumCO2=SumCO2_t0,
                         TA=TA_t0, SumSiOH4=0, SumBOH3=0,
                         SumH2SO4=0, SumHF=0)
        H_t0  <- 10^(-aetmp$pH)
        
        initialstate <<- c(SumCO2=SumCO2_t0, H=H_t0)
        times        <<- seq(0,modeltime,(modeltime/outputsteps))       
        output       <<- as.data.frame(vode(initialstate,times,
                                    boxmodel,parameters, hmax=1))
      })

what   <- c("SumCO2", "TA", "Rc", "Rp",
            "omega_calcite", "pH", "dHRc", "dHRp")
plot(aquaenv(ae=output, from.data.frame=TRUE), xval=output$time,
     xlab="time/d", mfrow=c(3,3), size=c(15,10), what=what) 

what <- c("dHRc", "dHRp")
plot(aquaenv(ae=output, from.data.frame=TRUE), xval=output$time,
     xlab="time/d", what=what, ylab="mol-H/(kg-soln*d)",
     legendposition="topright", cumulative=TRUE) 
}}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\keyword{misc}


