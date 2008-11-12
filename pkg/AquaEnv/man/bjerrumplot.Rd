\name{bjerrumplot}
\alias{bjerrumplot}
\title{bjerrumplot}
\description{PRIVATE function: creates a bjerrumplot from the elements of an object of class aquaenv given in what; no return value, just side-effect}
\usage{bjerrumplot(aquaenv, what, log=FALSE, palette=NULL, device="x11",
            filename="aquaenv", size=c(12,10), ylim=NULL,
            lwd=2, xlab="free scale pH",
            mgp=c(1.8, 0.5, 0), mar=c(3,3,0.5,0.5), oma=c(0,0,0,0),
            legendposition="bottomleft", legendinset=0.05, legendlwd=4,
            bg="white", newdevice, ...)}
\arguments{
  \item{aquaenv }{object of class aquaenv}
  \item{what }{vector of names of elements of aquaenv that should be plotted; if not specified:  what <- c("CO2", "HCO3", "CO3", "BOH3", "BOH4", "OH", "H3PO4", "H2PO4", "HPO4", "PO4", "SiOH4", "SiOOH3", "SiO2OH2", "H2S", "HS", "S2min", "NH4", "NH3", "H2SO4", "HSO4", "SO4", "HF", "F", "HNO3", "NO3", "HNO2", "NO2")}
  \item{log }{should the plot be on a logarithmic y axis?}
  \item{palette }{a vector of colors to use in the plot (either numbers or names given in colors())}
  \item{device }{the device to plot on; default: "x11" (can also be "eps" or "pdf")}
  \item{filename }{filename to be used if "eps" or "pdf" is selected for device}
  \item{size }{the size of the plot device; default: 12 (width) by 10 (height) inches}
  \item{ylim }{standard plot parameter; if not supplied it will be calculated by range() of the elements to plot}
  \item{lwd }{standard plot parameter; width of the lines in the plot}
  \item{xlab }{x axis label}
  \item{mgp }{standard plot parameter; default: axis title on line 1.8, axis labels on line 0.5, axis on line 0}
  \item{mar }{standard plot parameter; default: margin of 3 lines bottom and left and 0.5 lines top and right}
  \item{oma }{standard plot parameter; default: no outer margin}
  \item{legendposition }{position of the legend}
  \item{legendinset }{standard legend parameter inset}
  \item{legendlwd }{standard legend parameter lwd: line width of lines in legend}
  \item{bg }{standard legend parameter: default background color: white}
  \item{newdevice}{flag: if TRUE, new plot device is opened}
  \item{... }{further arguments will be passed}
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\keyword{IO}

