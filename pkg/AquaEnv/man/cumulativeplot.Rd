\name{cumulativeplot}
\alias{cumulativeplot}
\title{cumulativeplot}
\description{PRIVATE function: creates a cumulative plot from the elements of an object of class aquaenv given in what; no return value, just side-effect}
\usage{cumulativeplot(aquaenv, xval, what, total=TRUE, palette=NULL, device="x11", filename="aquaenv", size=c(12,10), ylim=NULL, lwd=2, mgp=c(1.8, 0.5, 0), mar=c(3,3,0.5,0.5), oma=c(0,0,0,0), legendposition="bottomleft", legendinset=0.05, legendlwd=4, bg="white", y.intersp=1.2, ...)}
\arguments{
  \item{aquaenv }{object of class aquaenv}
  \item{xval }{x-value: the independent variable describing a change in elements of an object of class aquaenv}
  \item{what }{vector of names of elements of aquaenv that should be plotted}
  \item{total }{should the sum of all elements specified in what be plotted as well?}
  \item{palette }{a vector of colors to use in the plot (either numbers or names given in colors())}
  \item{device }{the device to plot on;       default: "x11" (can also be "eps" or "pdf")}
  \item{filename }{filename to be used if "eps" or "pdf" is selected for device}
  \item{size }{the size of the plot device; default: 12 (width) by 10 (height) inches}
  \item{ylim }{standard plot parameter; if not supplied it will be calculated by an adaptation of range() of the elements to plot}
  \item{lwd }{standard plot parameter; width of the lines in the plot}
  \item{mgp }{standard plot parameter; default: axis title on line 1.8, axis labels on line 0.5, axis on line 0}
  \item{mar }{standard plot parameter; default: margin of 3 lines bottom and left and 0.5 lines top and right}
  \item{oma }{standard plot parameter; default: no outer margin}
  \item{legendposition }{position of the legend}
  \item{legendinset }{standard legend parameter inset}
  \item{legendlwd }{standard legend parameter lwd: line width of lines in legend}
  \item{bg }{standard legend parameter: default background color: white}
  \item{y.intersp }{standard legend parameter; default: 1.2 lines space between the lines in the legend}
  \item{... }{further arguments will be passed}
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\keyword{IO}

