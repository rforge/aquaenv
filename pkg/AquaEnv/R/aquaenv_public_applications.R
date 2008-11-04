# PUBLIC function:
# creates an object of class aquaenv which contains a titration simulation
titration <- function(aquaenv,                # an object of type aquaenv: minimal definition, contains all information about the system: T, S, d, total concentrations of nutrients etc (Note that it is possible to give values for SumBOH4, SumHSO4, and SumHF in the sample other than the ones calculated from salinity)
                      mass_sample,            # the mass of the sample solution in kg
                      mass_titrant,           # the total mass of the added titrant solution in kg
                      conc_titrant,           # the concentration of the titrant solution in mol/kg-soln
                      S_titrant=NULL,         # the salinity of the titrant solution, if not supplied it is assumed that the titrant solution has the same salinity as the sample solution
                      steps,                  # the amount of steps the mass of titrant is added in 
                      type,                   # the type of titrant: either "HCl" or "NaOH"
                      K1=NULL,                # used for TA fitting: give a K_CO2 and NOT calculate it from T and S: i.e. K_CO2 can be fitted in the routine as well
                      seawater_titrant=FALSE) # is the titrant based on natural seawater? (does it contain SumBOH4, SumHSO4, and SumHF in the same proportions as seawater, i.e., correlated to S?); Note that you can only assume a seawater based titrant (i.e. SumBOH4, SumHSO4, and SumHF ~ S) or a water based titrant (i.e. SumBOH4, SumHSO4, and SumHF = 0). It is not possible to give values for SumBOH4, SumHSO4, and SumHF of the titrant. 
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
               if (seawater_titrant)
                 {
                   SumBOH3  <- (SumBOH3[[i]]*mass[[i]]  + seaconc("B", S_titrant)*masschangeperstep)/newmass
                   SumH2SO4 <- (SumH2SO4[[i]]*mass[[i]] + seaconc("SO4", S_titrant)*masschangeperstep)/newmass
                   SumHF    <- (SumHF[[i]]*mass[[i]]    + seaconc("F", S_titrant)*masschangeperstep)/newmass
                 }
               else
                 {
                   SumBOH3  <- concstep(SumBOH3[[i]],  mass[[i]], newmass)
                   SumH2SO4 <- concstep(SumH2SO4[[i]], mass[[i]], newmass)
                   SumHF    <- concstep(SumHF[[i]],    mass[[i]], newmass)
                 }
               
               S           <- (S[[i]]*mass[[i]] + S_titrant*masschangeperstep)/newmass
               
               TA          <- (TA[[i]]*mass[[i]] + (directionTAchange * TAmolchangeperstep))/newmass

               aquaenvtemp <- aquaenv(Tc=Tc[[i]], S=S, d=d[[i]], SumCO2=SumCO2, SumNH4=SumNH4, SumH2S=SumH2S, SumH3PO4=SumH3PO4, SumSiOH4=SumSiOH4, SumHNO3=SumHNO3, SumHNO2=SumHNO2,
                                      SumBOH3=SumBOH3, SumH2SO4=SumH2SO4, SumHF=SumHF, TA=TA,
                                      speciation=(!is.null(aquaenv$HCO3)), skeleton=(is.null(aquaenv$Na)), revelle=(!is.null(aquaenv$revelle)), dsa=(!is.null(aquaenv$dTAdH)), K1=K1)

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
                  titcurve,                   # a table containing the titration curve: basically a series of tuples of added titrant solution mass and pH values (pH on free proton scale) or E values in V
                  conc_titrant,               # concentration of the titrant solution in mol/kg-soln
                  mass_sample,                # the mass of the sample solution in kg
                  S_titrant=NULL,             # the salinity of the titrant solution, if not supplied it is assumed that the titrant solution has the same salinity as the sample solution
                  TASumCO2guess=0.0025,       # a first guess for [TA] and [SumCO2] to be used as initial values for the optimization procedure
                  E0guess=0.4,                # first guess for E0 in V
                  type="HCl",                 # the type of titrant: either "HCl" or "NaOH"
                  Evals=FALSE,                # are the supplied datapoints pH or E (V) values?
                  K1fit=FALSE,                # should K1 be fitted as well?
                  equalspaced=TRUE,           # are the mass values of titcurve equally spaced?
                  seawater_titrant=FALSE)     # is the titrant based on natural seawater? (does it contain SumBOH4, SumHSO4, and SumHF in the same proportions as seawater, i.e., correlated to S?); Note that you can only assume a seawater based titrant (i.e. SumBOH4, SumHSO4, and SumHF ~ S) or a water based titrant (i.e. SumBOH4, SumHSO4, and SumHF = 0). It is not possible to give values for SumBOH4, SumHSO4, and SumHF of the titrant. 
  {
    ae$Na <- NULL   # make sure ae gets cloned as "skeleton": "skeleton" TRUE or FALSE is determined from the presence of a value for Na

    residuals <- function(state)
      {
        if (K1fit) {if (Evals){K1 <- state[[4]]/1e4} else {K1 <- state[[3]]/1e4}} else {K1 <- NULL}

        ae$SumCO2  <- state[[1]]
        tit        <- titration(aquaenv(ae=ae, TA=state[[2]], K1=K1), mass_sample=mass_sample, mass_titrant=titcurve[,1][[length(titcurve[,1])]],
                                conc_titrant=conc_titrant, S_titrant=S_titrant, steps=(length(titcurve[,1])-1), type=type, K1=K1, seawater_titrant=seawater_titrant)    
        calc       <- tit$pH
       
        if (Evals)
          {
            calc <- state[[3]] - ((Constants$R/10)*ae$Tk/Constants$F)*log(calc) #Nernst Equation: E = E0 -(RT/F)ln([H+]); 83.14510 (bar*cm3)/(mol*K) = 8.314510 J/(mol*K): division by 10
          }
        if (!equalspaced)
          {
            # to cope with not equally spaced delta values for the masses (i.e. per step of the titration, NOT the same amount of titrant has been added),
            # we fit a function through our calculated pH curve (NOT the data)
            calcfun <- approxfun(tit$delta_mass_titrant, calc, rule=2)

            residuals <- titcurve[,2]-calcfun(titcurve[,1])
          }
        else
          {     
            residuals <- titcurve[,2]-calc
          }
       
        return(residuals)
      }

    require(minpack.lm)
       
    if (Evals && K1fit)
      {
        out    <- nls.lm(fn=residuals, par=c(TASumCO2guess, TASumCO2guess, E0guess, ae$K_CO2*1e4)) #multiply K_CO2 with 1e4 to bring the variables to fit into the same order of magnitude
        result <- list(out$par[[2]], out$par[[1]], out$par[[3]], out$par[[4]]/1e4, out$deviance)
        
        attr(result[[3]], "unit")     <- "V"
        attr(result[[4]], "unit")     <- "mol/kg-soln"
        attr(result[[4]], "pH scale") <- "free"
        names(result)                 <- c("TA", "SumCO2", "E0", "K_CO2", "sumofsquares")    
      }
    else if (Evals)
      { 
        out    <- nls.lm(fn=residuals, par=c(TASumCO2guess, TASumCO2guess, E0guess))
        result <- list(out$par[[2]], out$par[[1]], out$par[[3]], out$deviance)
        
        attr(result[[3]], "unit") <- "V"
        names(result)             <- c("TA", "SumCO2", "E0", "sumofsquares")    
      }
    else if (K1fit)
      {    
        out    <- nls.lm(fn=residuals, par=c(TASumCO2guess, TASumCO2guess, ae$K_CO2*1e4)) #multiply K_CO2 with 1e4 to bring the variables to fit into the same order of magnitude
        result <- list(out$par[[2]], out$par[[1]], out$par[[3]]/1e4, out$deviance)
        
        attr(result[[3]], "unit")     <- "mol/kg-soln"
        attr(result[[3]], "pH scale") <- "free"
        names(result)                 <- c("TA", "SumCO2", "K_CO2", "sumofsquares")    
      }
    else
      {
        out    <- nls.lm(fn=residuals, par=c(TASumCO2guess, TASumCO2guess))
        result <- list(out$par[[2]], out$par[[1]], out$deviance)
        
        names(result) <- c("TA","SumCO2","sumofsquares")
      }
    
    attr(result[[1]], "unit")     <- "mol/kg-soln"
    attr(result[[2]], "unit")     <- "mol/kg-soln"    

    
    return(result)  # a list of up to five values ([TA] in mol/kg-solution, [SumCO2] in mol/kg-solution, E0 in V, K1 in mol/kg-solution and on free scale, sum of the squared residuals)
  }
