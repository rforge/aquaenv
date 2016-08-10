\name{Technicals}
\alias{Technicals}
\title{Technicals}
\description{PUBLIC list: a collection programming-technical constants}
\value{A list with elements:
   	\item{Haccur}{accuracy for iterative (Follows2006) pH calculations (max. deviation in [H+])}
        \item{Hstart}{start [H+] for an iterative pH calculation}
        \item{maxiter}{maximum number of iterations for iterative (Follows2006) pH calculation method as well as for the application of the standard R function uniroot}
        \item{unirootinterval}{the interval (in terms of [H+]) for pH calculation using the standard R function uniroot}
        \item{uniroottol}{the interval (in terms of [H+]) for pH calculation using the standard R function uniroot}
        \item{epsilon_fraction}{fraction of disturbance for the numerical calculation of derivatives of TA with respect to changes in the dissociation constants}
        \item{revelle_fraction}{fraction of disturbance for the numerical calculation of the revelle factor}
	\item{CO2}{fugacity of CO2 in atm}
       } 
\author{Andreas F. Hofmann. Maintained by Karline Soetaert (Karline.Soetaert@nioz.nl).}
\keyword{misc}

