# PUBLIC function:
# creates an object of class aquaenv which contains a titration simulation 
titration <- function(aquaenv,                # an object of type aquaenv: minimal definition, contains all information about the system: T, S, d, total concentrations of nutrients etc 
                      mass_sample,            # the mass of the sample solution in kg
                      mass_titrant,           # the total mass of the added titrant solution in kg
                      conc_titrant,           # the concentration of the titrant solution in mol/kg-soln
                      S_titrant=NULL,         # the salinity of the titrant solution, if not supplied it is assumed that the titrant solution has the same salinity as the sample solution
                      steps,                  # the amount of steps the mass of titrant is added in 
                      type)                   # the type of titrant: either "HCl" or "NaOH"
  {
    concstep <- function(conc, oldmass, newmass)
      {
        return((conc*oldmass)/(newmass))
      }

    if (is.null(S_titrant))
      {
        S_titrant <- aquaenv$S
      }
    
    directionTAchange   <- switch(type, HCl  = -1, NaOH = +1)
    TAmolchangeperstep  <- (conc_titrant*mass_titrant)/steps
    masschangeperstep   <- mass_titrant/steps

    aquaenv[["mass"]]   <- mass_sample
    
    for (i in 1:steps)
      {
        with(aquaenv,
             {
               i           <- length(mass)
               
               newmass     <- mass[[i]] + masschangeperstep
               
               SumCO2      <- concstep(SumCO2[[i]],   mass[[i]], newmass)
               SumNH4      <- concstep(SumNH4[[i]],   mass[[i]], newmass)
               SumH2S      <- concstep(SumH2S[[i]],   mass[[i]], newmass)
               SumH3PO4    <- concstep(SumH3PO4[[i]], mass[[i]], newmass)
               SumSiOH4    <- concstep(SumSiOH4[[i]], mass[[i]], newmass)
               SumHNO3     <- concstep(SumHNO3[[i]],  mass[[i]], newmass)
               SumHNO2     <- concstep(SumHNO2[[i]],  mass[[i]], newmass)
               SumBOH3     <- concstep(SumBOH3[[i]],  mass[[i]], newmass)
               SumH2SO4    <- concstep(SumH2SO4[[i]], mass[[i]], newmass)
               SumHF       <- concstep(SumHF[[i]],    mass[[i]], newmass)
               
               S           <- (S[[i]]*mass[[i]] + S_titrant*masschangeperstep)/newmass
               
               TA          <- (TA[[i]]*mass[[i]] + (directionTAchange * TAmolchangeperstep))/newmass

               aquaenvtemp <- aquaenv(Tc=Tc[[i]], S=S, d=d[[i]], SumCO2=SumCO2, SumNH4=SumNH4, SumH2S=SumH2S, SumH3PO4=SumH3PO4, SumSiOH4=SumSiOH4, SumHNO3=SumHNO3, SumHNO2=SumHNO2,
                                      SumBOH3=SumBOH3, SumH2SO4=SumH2SO4, SumHF=SumHF, TA=TA,
                                      speciation=(!is.null(aquaenv$HCO3)), skeleton=(is.null(aquaenv$Na)), revelle=(!is.null(aquaenv$revelle)), dsa=(!is.null(aquaenv$dTAdH)))

               aquaenvtemp[["mass"]] <- newmass
               aquaenv               <<- merge(aquaenv, aquaenvtemp)              
             })
        }

    aquaenv[["delta_conc_titrant"]]   <- ((0:steps)*TAmolchangeperstep)/aquaenv[["mass"]] ; attr(aquaenv[["delta_conc_titrant"]], "unit")  <- "mol/kg-soln"
    aquaenv[["delta_mass_titrant"]]   <- (0:steps)*masschangeperstep                      ; attr(aquaenv[["delta_mass_titrant"]], "unit")  <- "kg"
    aquaenv[["delta_moles_titrant"]]  <- (0:steps)*TAmolchangeperstep                     ; attr(aquaenv[["delta_moles_titrant"]], "unit") <- "mol"

    return(aquaenv)                           # object of class aquaenv which contains a titration simulation
  }



# PUBLIC function:
# calculates [TA] and [SumCO2] from a titration curve using an optimization procedure (nls.lm from R package minpack.lm)
TAfit <- function(ae,                         # an object of type aquaenv: minimal definition, contains all information about the system: T, S, d, total concentrations of nutrients etc 
                  pHmeasurements,             # a table containing the titration curve: basically a series of pH values (pH on free proton scale)
                  volume,                     # the volume of the titration vessel
                  amount,                     # the total amount of the titrant added
                  TAguess=0.0025,             # a first guess for [TA] and [SumCO2] to be used as initial values for the optimization procedure
                  type="HCl")                 # the type of titrant: either "HCl" or "NaOH"
  {
    ae$Na <- NULL   # make sure ae gets cloned as "skeleton": cloneaquaenv determines "skeleton" TRUE or FALSE from the presence of a value for Na
    sumsquares <- function(state)
      {
        ae$SumCO2  <- state[[1]]
        pHcalc     <- titration(aquaenv(ae=ae, TA=state[[2]]), volume=volume, amount=amount, steps=(length(pHmeasurements)-1), type=type)$pH
        sumsquares <- pHmeasurements-pHcalc
       
        return(sumsquares)
      }

    require(minpack.lm)
    out <- nls.lm(fn=sumsquares, par=c(TAguess, TAguess))  #guess for TA is also used as guess for SumCO2
  
    result                    <- list(out$par[[2]], out$par[[1]], out$deviance)
    attr(result[[1]], "unit") <- "mol/kg-soln"
    attr(result[[2]], "unit") <- "mol/kg-soln"
    names(result)             <- c("TA", "SumCO2", "sumofsquares")
    return(result)                            # a list of three values ([TA] in mol/kg-solution, [SumCO2] in mol/kg-solution, sum of the squared residuals)
  }
