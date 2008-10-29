# PUBLIC function:
# creates an object of class aquaenv which contains a titration simulation (changes in [Na] and [Cl] and therefore in S are assumed negligible (~ 5 mmol/kg wrt 500 mmol/kg))
titration <- function(aquaenv,                # an object of type aquaenv: minimal definition, contains all information about the system: T, S, d, total concentrations of nutrients etc 
                      volume,                 # the volume of the (theoretical) titration vessel in l 
                      amount,                 # the amount of titrant added in mol
                      steps,                  # the amount of steps the amount of titrant is added in 
                      type)                   # the type of titrant: either "HCl" or "NaOH"
  {
    directionTAchange   <- switch(type, HCl  = -1, NaOH = +1)
    TAconcchangeperstep <- convert(((amount/steps)/volume), "conc", "molar2molin", aquaenv$Tc, aquaenv$S)

    aquaenvtemp <- aquaenv
    
    for (i in 1:steps)
      {
        TA          <- aquaenvtemp$TA + (directionTAchange * TAconcchangeperstep)
        aquaenvtemp <- cloneaquaenv(aquaenvtemp, TA=TA)
        aquaenv     <- merge(aquaenv, aquaenvtemp)
      }

    aquaenv[["DeltaCTitrant"]] <- convert((amount/volume)/steps*(1:(steps+1)), "conc", "molar2molin", aquaenv$Tc, aquaenv$S)
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
