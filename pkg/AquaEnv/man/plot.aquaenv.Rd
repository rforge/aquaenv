\name{plot.aquaenv}
\alias{plot.aquaenv}
\title{plot.aquaenv}
\description{PUBLIC function: high level plot function for objects of  class aquaenv}
\details{Top level generic usage is
  \preformatted{plot.aquaenv(x, xval, what=NULL, bjerrum=FALSE, cumulative=FALSE, newdevice=TRUE,...)}
  Generic usages for standard plotting are
  \preformatted{plot.aquaenv(x, xval, ...)}
  \preformatted{plot.aquaenv(x, xval, what, mfrow=c(1,1), size=c(7,7), ...)}
  Generic usage for creating a bjerrum plot is
  \preformatted{plot.aquaenv(x, what, log=FALSE, palette=NULL, device="x11", filename="aquaenv", size=c(12,10), ylim=NULL, lwd=2, xlab="free scale pH", mgp=c(1.8, 0.5, 0), mar=c(3,3,0.5,0.5), oma=c(0,0,0,0), legendposition="bottomleft", legendinset=0.05, legendlwd=4, bg="white", newdevice=TRUE, ...)}
  Generic usage for creating a cumulative plot is
  \preformatted{plot.aquaenv(x, xval, what, total=TRUE, palette=NULL, device="x11", filename="aquaenv", size=c(12,10), ylim=NULL, lwd=2, mgp=c(1.8, 0.5, 0), mar=c(3,3,0.5,0.5), oma=c(0,0,0,0), legendposition="bottomleft", legendinset=0.05, legendlwd=4, bg="white", y.intersp=1.2, newdevice, ...)}
}
\arguments{
  \item{x}{object of class aquaenv}
  \item{xval }{only valid if bjerrum=FALSE: a vector of the (maximal) length of the elements of aquaenv against which they are to be plotted}
  \item{what }{a list of names of the elements of aquaenv that are to be plotted, if not supplied and bjerrum=FALSE and cumulative=FALSE: all elements are plotted, if not supplied and bjerrum=TRUE then what is set to be c("CO2", "HCO3", "CO3", "BOH3", "BOH4", "OH", "H3PO4", "H2PO4", "HPO4", "PO4", "SiOH4", "SiOOH3", "SiO2OH2", "H2S", "HS", "S2min", "NH4", "NH3", "H2SO4", "HSO4", "SO4", "HF", "F", "HNO3", "NO3", "HNO2", "NO2"), needs to be supplied for cumulative=TRUE}
  \item{bjerrum }{flag: TRUE = a bjerrum plot is done (by calling bjerrumplot)}
  \item{cumulative }{flag: TRUE = a cumulative plot is done (by calling cumulativeplot)}
  \item{newdevice}{flag: if TRUE, new plot device is opened}
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
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\keyword{misc}


