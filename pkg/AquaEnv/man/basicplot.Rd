\name{basicplot}
\alias{basicplot}
\title{basicplot}
\description{PRIVATE function: basic wrapper for the R plot function for plotting objects of class aquaenv; no return value, just side-effect}
\usage{basicplot(aquaenv, xval, type="l", mgp=c(1.8, 0.5, 0),
          mar=c(3,3,0.5,0.5), oma=c(0,0,0,0), size=c(15,13),
          mfrow=c(11,10), device="x11", filename="aquaenv",
          newdevice,...)}
\arguments{
  \item{aquaenv }{object of class aquaenv}
  \item{xval }{x-value: the independent variable describing a change in elements of an object of class aquaenv}
  \item{type }{standard plot parameter;     default: plot lines}
  \item{mgp }{standard plot parameter;     default: axis title on line 1.8, axis labels on line 0.5, axis on line 0}
  \item{mar }{standard plot parameter;     default: margin of 3 lines bottom and left and 0.5 lines top and right}
  \item{oma }{standard plot parameter;     default: no outer margin}
  \item{size }{the size of the plot device; default: 15 (width) by 13 (height) inches}
  \item{mfrow }{standard plot parameter;     default: 11 columns and 10 rows of plots}
  \item{device }{the device to plot on;       default: "x11" (can also be "eps" or "pdf")}
  \item{filename }{filename to be used if "eps" or "pdf" is selected for
    device}
  \item{newdevice}{flag: if TRUE, new plot device is opened}
  \item{... }{further arguments will be passed}
}
\author{Andreas F. Hofmann (a.hofmann@nioo.knaw.nl)}
\keyword{IO}

