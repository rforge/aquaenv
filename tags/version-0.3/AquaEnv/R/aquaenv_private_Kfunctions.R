#######################################################################################
# local functions
#######################################################################################
lnK <- function(A, B, C, D, E, Tk)        
  {
    lnK <- A + (B/Tk) + C*log(Tk) + D*Tk + E*Tk^2
  }

logK <- function(A, B, C, D, E, Tk)        
  {
    logK <- A + (B/Tk) + C*log10(Tk) + D*Tk + E*Tk^2
  }

Sterms <- function(S)
  {
    sqrtS  <- sqrt(S)
    Sterms <- list(S^2, sqrtS, S*sqrtS)
  }

Iterms <- function(S)
  {
    I      <- I(S)
    sqrtI  <- sqrt(I)
    Iterms <- list(I, I^2, sqrtI, I*sqrtI)
  }

deltaPlnK <- function(Tk, d, coeff)
  {
    P         <- hydroP(d)
    Tc        <- Tk + Constants$absZero 
    Tc2       <- Tc^2
    deltaV    <- coeff[[1]] + coeff[[2]]*Tc + coeff[[3]]*Tc2
    deltaK    <- (coeff[[4]] + coeff[[5]]*Tc + coeff[[6]]*Tc2)/1000 #the 1000 is correction from Lewis1998
    deltaPlnK <- -(deltaV/(Constants$R*Tk))*P + (0.5*(deltaK/(Constants$R*Tk)))*(P^2)  
  }

splitS_K_CO2 <- function(Tk)
  {
    res <- c()
    for (t in Tk)
      {
        lnK1 <- function(Tk,S)
          {
            Sterms <- Sterms(S)
            
            A <- 2.83655    - 0.20760841*Sterms[[2]] + 0.08468345*S - 0.00654208*Sterms[[3]]
            B <- -2307.1266 -     4.0484*Sterms[[2]]
            C <- -1.5529413
            
            lnK1 <- lnK(A, B, C, 0, 0, Tk)    
          }
        
        lnK2 <- function(Tk,S)
          {
            Sterms <- Sterms(S)
            
            A <- 290.9097  -  228.39774*Sterms[[2]] +   54.20871*S -  3.969101*Sterms[[3]] - 0.00258768*Sterms[[1]]
            B <- -14554.21 + 9714.36839*Sterms[[2]] - 2310.48919*S + 170.22169*Sterms[[3]]
            C <- -45.0575  +  34.485796*Sterms[[2]] -    8.19515*S +   0.60367*Sterms[[3]]
            
            lnK2 <- lnK(A, B, C, 0, 0, Tk)    
          }
        res <- c(res, uniroot(function(x) lnK1(t,x)-lnK2(t,x), c(3,7))$root)
      }
    return(res)
  }

splitS_K_HCO3 <- function(Tk)
  {
    res <- c()
    for (t in Tk)
      {
        lnK1 <- function(Tk,S)
          {
            Sterms <- Sterms(S)
            
            A <- -9.226508  - 0.106901773*Sterms[[2]] + 0.1130822*S - 0.00846934*Sterms[[3]] 
            B <- -3351.6106 -     23.9722*Sterms[[2]] 
            C <- -0.2005743 
            
            lnK1 <- lnK(A, B, C, 0, 0, Tk)    
          }
        
        lnK2 <- function(Tk,S)
          {
            Sterms <- Sterms(S)
            
            A <- 207.6548  -  167.69908*Sterms[[2]] +   39.75854*S -   2.892532*Sterms[[3]] - 0.00613142*Sterms[[1]]
            B <- -11843.79 + 6551.35253*Sterms[[2]] - 1566.13883*S + 116.270079*Sterms[[3]]
            C <- -33.6485  +  25.928788*Sterms[[2]] -   6.171951*S + 0.45788501*Sterms[[3]]
            
            lnK2 <- lnK(A, B, C, 0, 0, Tk)    
          }
        res <- c(res, uniroot(function(x) lnK1(t,x)-lnK2(t,x), c(3,7))$root)
      }
    return(res)
  }

att <- function(K)
  {
    attr(K, "unit")     <- "mol/kg-soln"
    attr(K, "pH scale") <- "free"
    att                 <- K
  }
######################################################################################





######################################################################################
# Henry's constants
######################################################################################
K0_CO2 <- function(Tc, S)           
  {
    Tk <- Tk(Tc)

    A <- 0.023517*S - 167.81077
    B <- 9345.17
    C <- 23.3585
    D <- -2.3656e-4*S
    E <- 4.7036e-7*S
    
    K0_CO2 <- exp(lnK(A, B, C, D, E, Tk))
    attr(K0_CO2, "unit") <- "mol/(kg-soln*atm)"
    
    return (K0_CO2)
  }

K0_O2 <- function(Tc, S)            
  {
    Tk <- Tk(Tc) 
    
    A <- -846.9975 - 0.037362*S
    B <- 25559.07 
    C <- 146.4813
    D <- -0.22204 + 0.00016504*S
    E <- -2.0564e-7*S
    
    K0_O2 <- exp(lnK(A, B, C, D, E, Tk))
    K0_O2 <- K0_O2 * Constants$uMolToMol
    attr(K0_O2, "unit") <- "mol/(kg-soln*atm)"

    return(K0_O2)
  }
######################################################################################




######################################################################################
# ion product of water
######################################################################################
K_W <- function(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)
  {
    Tk <- Tk(Tc) 
    Sterms <- Sterms(S)
    
    A <- 148.9652  -  5.977*Sterms[[2]] - 0.01615*S
    B <- -13847.26 + 118.67*Sterms[[2]]
    C <- -23.6521  + 1.0495*Sterms[[2]]
    D <- 0
    E <- 0
    
    K_W <- exp(lnK(A, B, C, D, E, Tk) + deltaPlnK(Tk, d, DeltaPcoeffs$K_W) + log(scaleconvert(Tc, S, d, SumH2SO4, SumHF)$tot2free))
    attr(K_W, "unit")     <- "(mol/kg-soln)^2"
    attr(K_W, "pH scale") <- "free" 
    
    return(K_W)
  }
######################################################################################




######################################################################################
# acid dissociation constants
######################################################################################
K_HSO4 <- function(Tc, S, d=0)           
  {
    Tk <- Tk(Tc)
    Iterms <- Iterms(S)
    
    A <-  324.57*Iterms[[3]] - 771.54*Iterms[[1]] + 141.328  
    B <-   35474*Iterms[[1]] +   1776*Iterms[[2]] - 13856*Iterms[[3]] - 2698*Iterms[[4]] - 4276.1
    C <- 114.723*Iterms[[1]] - 47.986*Iterms[[3]] - 23.093
    D <- 0
    E <- 0
    
    K_HSO4 <- exp(lnK(A, B, C, D, E, Tk) + deltaPlnK(Tk, d, DeltaPcoeffs$K_HSO4) + log(molal2molin(S)))

    return (eval(att(K_HSO4)))
  }

K_HF <- function(Tc, S, d=0)
  {
    Tk <- Tk(Tc)
    Iterms <- Iterms(S)
    
    A <- 1.525 * Iterms[[3]] - 12.641
    B <- 1590.2
    C <- 0
    D <- 0
    E <- 0
    
    K_HF <- exp(lnK(A, B, C, D, E, Tk) + deltaPlnK(Tk, d, DeltaPcoeffs$K_HF) + log(molal2molin(S)))
    
    return(eval(att(K_HF)))
  }

K_CO2 <- function(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)
  {
    Tk     <- Tk(Tc)
    splitS <- splitS_K_CO2(Tk)
    K_CO2  <- c()
    
    for (s in 1:length(S))
      {
        Sterms <- Sterms(S[[s]])

        for (t in 1:length(Tc))
          {
            if (S[[s]] > splitS[[t]])
              {
                A <- 2.83655    - 0.20760841*Sterms[[2]] + 0.08468345*S[[s]] - 0.00654208*Sterms[[3]]
                B <- -2307.1266 -     4.0484*Sterms[[2]]
                C <- -1.5529413
                D <- 0
                E <- 0
              }
            else
              {
                A <- 290.9097  -  228.39774*Sterms[[2]] +   54.20871*S[[s]] -  3.969101*Sterms[[3]] - 0.00258768*Sterms[[1]]
                B <- -14554.21 + 9714.36839*Sterms[[2]] - 2310.48919*S[[s]] + 170.22169*Sterms[[3]]
                C <- -45.0575  +  34.485796*Sterms[[2]] -    8.19515*S[[s]] +   0.60367*Sterms[[3]]
                D <- 0
                E <- 0
              }
          }
        
        K_CO2 <- c(K_CO2, exp(lnK(A, B, C, D, E, Tk) +  deltaPlnK(Tk, d, DeltaPcoeffs$K_CO2) + log(scaleconvert(Tc, S[[s]], d, SumH2SO4[[s]], SumHF[[s]])$tot2free) + log(molal2molin(S[[s]]))))
      }
    
    return(eval(att(K_CO2)))
  }

K_HCO3 <- function(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)
  {
    Tk     <- Tk(Tc)
    splitS <- splitS_K_HCO3(Tk)
    K_HCO3 <- c()
    
    for (s in 1:length(S))
      {
        Sterms <- Sterms(S[[s]])

        for (t in 1:length(Tc))
          {
            if (S[[s]] > splitS[[t]])
              {
                A <- -9.226508  - 0.106901773*Sterms[[2]] + 0.1130822*S[[s]] - 0.00846934*Sterms[[3]] 
                B <- -3351.6106 -     23.9722*Sterms[[2]] 
                C <- -0.2005743 
                D <- 0
                E <- 0
              }
            else
              {
                A <- 207.6548  -  167.69908*Sterms[[2]] +   39.75854*S[[s]] -   2.892532*Sterms[[3]] - 0.00613142*Sterms[[1]]
                B <- -11843.79 + 6551.35253*Sterms[[2]] - 1566.13883*S[[s]] + 116.270079*Sterms[[3]]
                C <- -33.6485  +  25.928788*Sterms[[2]] -   6.171951*S[[s]] + 0.45788501*Sterms[[3]]
                D <- 0
                E <- 0
              }
          }
            
        K_HCO3 <- c(K_HCO3, exp(lnK(A, B, C, D, E, Tk) + deltaPlnK(Tk, d, DeltaPcoeffs$K_HCO3) + log(scaleconvert(Tc, S[[s]], d, SumH2SO4[[s]], SumHF[[s]])$tot2free) + log(molal2molin(S[[s]]))))
      }
    
    return(eval(att(K_HCO3)))
  }

K_BOH3 <- function(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)
  {
    Tk <- Tk(Tc)
    Sterms <- Sterms(S)
    
    A <- 148.0248 + 137.1942*Sterms[[2]] + 1.62142*S
    B <- -8966.90 -  2890.53*Sterms[[2]] -  77.942*S + 1.728*Sterms[[3]] - 0.0996*Sterms[[1]]
    C <- -24.4344 -   25.085*Sterms[[2]] -  0.2474*S
    D <-            0.053105*Sterms[[2]]
    E <- 0
    
    K_BOH3 <- exp(lnK(A, B, C, D, E, Tk) + deltaPlnK(Tk, d, DeltaPcoeffs$K_BOH3) + log(scaleconvert(Tc, S, d, SumH2SO4, SumHF)$tot2free))
    
    return(eval(att(K_BOH3)))
  }

K_NH4 <- function(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)
  {
    Tk <- Tk(Tc)
    Sterms <- Sterms(S)
    
    A <- -0.25444 +  0.46532*Sterms[[2]] - 0.01992*S
    B <- -6285.33 - 123.7184*Sterms[[2]] + 3.17556*S
    C <- 0
    D <- 0.0001635
    E <- 0
    
    K_NH4 <- exp(lnK(A, B, C, D, E, Tk) + deltaPlnK(Tk, d, DeltaPcoeffs$K_NH4) + log(scaleconvert(Tc, S, d, SumH2SO4, SumHF)$sws2free))
    
    return(eval(att(K_NH4)))
  }

K_H2S <- function(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)
  {
    Tk <- Tk(Tc)
    Sterms <- Sterms(S)
    
    A <- 225.838  + 0.3449*Sterms[[2]] - 0.0274*S
    B <- -13275.3
    C <- -34.6435
    D <- 0
    E <- 0
    
    K_H2S <- exp(lnK(A, B, C, D, E, Tk) + deltaPlnK(Tk, d, DeltaPcoeffs$K_H2S) + log(scaleconvert(Tc, S, d, SumH2SO4, SumHF)$tot2free))
        
    return(eval(att(K_H2S)))
  }

K_H3PO4 <- function(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)
  {
    Tk <- Tk(Tc)
    Sterms <- Sterms(S)
    
    A <- 115.525   + 0.69171*Sterms[[2]] - 0.01844*S
    B <- -4576.752 - 106.736*Sterms[[2]] - 0.65643*S
    C <- -18.453
    D <- 0
    E <- 0
    
    K_H3PO4 <- exp(lnK(A, B, C, D, E, Tk) + deltaPlnK(Tk, d, DeltaPcoeffs$K_H3PO4) + log(scaleconvert(Tc, S, d, SumH2SO4, SumHF)$tot2free))
    
    return(eval(att(K_H3PO4)))
  }

K_H2PO4 <- function(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL) 
  {
    Tk <- Tk(Tc)
    Sterms <- Sterms(S)
    
    A <- 172.0883  +  1.3566*Sterms[[2]] - 0.05778*S
    B <- -8814.715 - 160.340*Sterms[[2]] + 0.37335*S
    C <- -27.927
    D <- 0
    E <- 0
    
    K_H2PO4 <- exp(lnK(A, B, C, D, E, Tk) + deltaPlnK(Tk, d, DeltaPcoeffs$K_H2PO4) + log(scaleconvert(Tc, S, d, SumH2SO4, SumHF)$tot2free))
    
    return(eval(att(K_H2PO4)))
  }

K_HPO4 <- function(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)
  {
    Tk <- Tk(Tc)
    Sterms <- Sterms(S)
    
    A <- -18.141  +  2.81197*Sterms[[2]] -  0.09984*S
    B <- -3070.75 + 17.27039*Sterms[[2]] - 44.99486*S
    C <- 0
    D <- 0
    E <- 0 
    
    K_HPO4 <- exp(lnK(A, B, C, D, E, Tk) + deltaPlnK(Tk, d, DeltaPcoeffs$K_HPO4) + log(scaleconvert(Tc, S, d, SumH2SO4, SumHF)$tot2free))
    
    return(eval(att(K_HPO4)))
  }

K_SiOH4 <- function(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)
  {
    Tk <- Tk(Tc)
    Iterms <- Iterms(S)
    
    A <- 117.385 + 3.5913*Iterms[[3]] - 1.5998*Iterms[[1]] + 0.07871*Iterms[[2]]
    B <- -8904.2 - 458.79*Iterms[[3]] + 188.74*Iterms[[1]] - 12.1652*Iterms[[2]]
    C <- -19.334
    D <- 0
    E <- 0
    
    K_SiOH4 <- exp(lnK(A, B, C, D, E, Tk) + deltaPlnK(Tk, d, DeltaPcoeffs$K_SiOH4) + log(scaleconvert(Tc, S, d, SumH2SO4, SumHF)$tot2free) + log(molal2molin(S)))
    
    return(eval(att(K_SiOH4)))
  }

K_SiOOH3 <- function(Tc, S, d=0, SumH2SO4=NULL, SumHF=NULL)
  {
    Tk <- Tk(Tc)

    A <- 8.96
    B <- -4465.18
    C <- 0
    D <- 0.021952
    E <- 0
    
    K_SiOOH3 <- exp(lnK(A, B, C, D, E, Tk) + deltaPlnK(Tk, d, DeltaPcoeffs$K_SiOOH3) + log(scaleconvert(Tc, S, d, SumH2SO4, SumHF)$tot2free) + log(molal2molin(S)))
    
    return(eval(att(K_SiOOH3)))
  }
######################################################################################



######################################################################################
# solubility products
######################################################################################
Ksp_calcite <- function(Tc, S, d=0) 
  {
    Tk <- Tk(Tc)
    Sterms <- Sterms(S)
    
    A <- -171.9065 -   0.77712*Sterms[[2]] - 0.07711*S + 0.0041249*Sterms[[3]]
    B <- 2839.319  +    178.34*Sterms[[2]]
    C <- 71.595
    D <- -0.077993 + 0.0028426*Sterms[[2]]
    E <- 0
    
    Ksp_calcite <- 10^(logK(A, B, C, D, E, Tk)) * exp(deltaPlnK(Tk, d, DeltaPcoeffs$Ksp_calcite))  
    attr(Ksp_calcite, "unit") <- "(mol/kg-soln)^2"     

    return(Ksp_calcite)
  }

Ksp_aragonite <- function(Tc, S, d=0) 
  {
    Tk <- Tk(Tc)
    Sterms <- Sterms(S)
    
    A <- -171.945  -  0.068393*Sterms[[2]] - 0.10018*S + 0.0059415*Sterms[[3]]
    B <- 2903.293  +    88.135*Sterms[[2]]
    C <- 71.595
    D <- -0.077993 + 0.0017276*Sterms[[2]]
    E <- 0
    
    Ksp_aragonite <- 10^(logK(A, B, C, D, E, Tk)) * exp(deltaPlnK(Tk, d, DeltaPcoeffs$Ksp_aragonite))
    attr(Ksp_aragonite, "unit") <- "(mol/kg-soln)^2"     

    return(Ksp_aragonite)
  }
