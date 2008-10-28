# derivatives of species concentrations of a univalent acid with respect to [H+]
dHAdH_uni <- function(H, SumA, K)
{
  dHAdH_uni <- (1/(H+K) - H/(H^2 + 2*H*K + K^2))*SumA
}

dAdH_uni <- function(H, SumA, K)
{
  dAdH_uni <- - K/(H^2 + 2*H*K + K^2)*SumA
}


# derivatives of species concentrations of a bivalent acid with respect to [H+]
dH2AdH_bi <- function(H, SumA, K1, K2)
{
  dH2AdH_bi <- ((2*H/(H*K1 + K1*K2 + H^2)) - ((H^2*(2*H+K1))/(H*K1 + K1*K2 + H^2)^2)) * SumA
}

dHAdH_bi <- function(H, SumA, K1, K2)
{
  dHAdH_bi <- ((K1/(H*K1 + K1*K2 + H^2)) - ((H*K1*(2*H+K1))/(H*K1 + K1*K2 + H^2)^2)) * SumA
}

dAdH_bi <- function(H, SumA, K1, K2)
{
  dAdH_bi <- (- ((K1*K2*(2*H+K1))/(H*K1 + K1*K2 + H^2)^2)) * SumA
}


# derivatives of species concentrations of a trivalent acid with respect to [H+]
dH3AdH_tri <- function(H, SumA, K1, K2, K3)
{
  dH3AdH_tri <- ((3*H^2/(H*K1*K2 + K1*K2*K3 + H^2*K1 + H^3)) - ((H^3*(2*H*K1 + K1*K2 + 3*H^2))/(H*K1*K2 + K1*K2*K3 + +H^2*K1 + H^3)^2)) * SumA
}

dH2AdH_tri <- function(H, SumA, K1, K2, K3)
{
  dH2AdH_tri <- ((2*H*K1/(H*K1*K2 + K1*K2*K3 + H^2*K1 + H^3)) - ((H^2*K1*(2*H*K1 + K1*K2 + 3*H^2))/(H*K1*K2 + K1*K2*K3 + +H^2*K1 + H^3)^2)) * SumA
}

dHAdH_tri <- function(H, SumA, K1, K2, K3)
{
  dHAdH_tri <- ((K1*K2/(H*K1*K2 + K1*K2*K3 + H^2*K1 + H^3)) - ((H*K1*K2*(2*H*K1 + K1*K2 + 3*H^2))/(H*K1*K2 + K1*K2*K3 + +H^2*K1 + H^3)^2)) * SumA
}

dAdH_tri <- function(H, SumA, K1, K2, K3)
{
  dAdH_tri <- (- ((K1*K2*K3*(2*H*K1 + K1*K2 + 3*H^2))/(H*K1*K2 + K1*K2*K3 + +H^2*K1 + H^3)^2)) * SumA
}


# PRIVATE function:
# calculates the derivative of [TA] with respect to [H+]: the buffer factor 
dTAdH <- function(ae)                         # object of class aquaenv
  {
    with (ae,
          {
            H      <- 10^(-pH)
            
            res  <-  (      dHAdH_bi  (H, SumCO2, K_CO2, K_HCO3)
                      + 2 * dAdH_bi   (H, SumCO2, K_CO2, K_HCO3)
                      +             (- K_W/H^2)
                      +     dAdH_uni  (H, SumBOH3, K_BOH3)
                      +     dHAdH_tri (H, SumH3PO4, K_H3PO4, K_H2PO4, K_HPO4)
                      + 2 * dAdH_tri  (H, SumH3PO4, K_H3PO4, K_H2PO4, K_HPO4)
                      +     dH2AdH_bi (H, SumSiOH4, K_SiOH4, K_SiOOH3)
                      + 2 * dHAdH_bi  (H, SumSiOH4, K_SiOH4, K_SiOOH3)
                      +     dAdH_uni  (H, SumNH4, K_NH4) 
                      +     dHAdH_bi  (H, SumH2S, K_H2S, K_HS)
                      + 2 * dAdH_bi   (H, SumH2S, K_H2S, K_HS)
                      -              1
                      -     dH3AdH_tri(H, SumH3PO4, K_H3PO4, K_H2PO4, K_HPO4)
                      -     dHAdH_bi  (H, SumH2SO4, K_H2SO4, K_HSO4)
                      -     dHAdH_uni (H, SumHF, K_HF)
                      -     dHAdH_uni (H, SumHNO3, K_HNO3)
                      -     dHAdH_uni (H, SumHNO2, K_HNO2)
                      - 2 * dH2AdH_bi (H, SumH2SO4, K_H2SO4, K_HSO4)
                      )
            
             return (res)                     # derivative of [TA] with respect to [H+]: the buffer factor 
          })    
  }


# PRIVATE function:
# calculates the derivative of [TA] with respect to changes in the dissociation constants (Ks) times the derivative of the dissociation constants with respect to salinity S
dTAdKdKdS <- function(ae)                     # object of class aquaenv
  {
    with (ae,
          {
            epsilon <- S * Technicals$epsilon_fraction

            TAplus <- c()
            TAminus <- c()
            
            for (s in 1:length(S))
              {
                for (de in 1:length(d))
                  {
                    if (length(pH)     > 1) {i <- max(s,de)} else {i <- 1}
                    if (length(SumCO2) > 1) {x <- max(s,de)} else {x <- 1}
                    TAplus  <- c(TAplus, aquaenv(Tc=Tc, S=(S[[s]]+epsilon[[s]]), d=d[[de]],
                                                 SumCO2=SumCO2[[x]], SumNH4=SumNH4, SumH2S=SumH2S, SumH3PO4=SumH3PO4, SumSiOH4=SumSiOH4, SumHNO3=SumHNO3, SumHNO2=SumHNO2, 
                                                 SumBOH3=SumBOH3[[s]], SumH2SO4=SumH2SO4[[s]], SumHF=SumHF[[s]],
                                                 TA=NULL, pH=pH[[i]], pCO2=NULL, CO2=NULL, speciation=FALSE, dsa=FALSE, ae=NULL, from.data.frame=FALSE,  revelle=FALSE)$TA)
                    TAminus <- c(TAminus, aquaenv(Tc=Tc, S=(S[[s]]-epsilon[[s]]), d=d[[de]],
                                                  SumCO2=SumCO2[[x]], SumNH4=SumNH4, SumH2S=SumH2S, SumH3PO4=SumH3PO4, SumSiOH4=SumSiOH4, SumHNO3=SumHNO3, SumHNO2=SumHNO2, 
                                                  SumBOH3=SumBOH3[[s]], SumH2SO4=SumH2SO4[[s]], SumHF=SumHF[[s]],
                                                  TA=NULL, pH=pH[[i]], pCO2=NULL, CO2=NULL, speciation=FALSE, dsa=FALSE, ae=NULL, from.data.frame=FALSE,  revelle=FALSE)$TA)
                  }
              }
            return ((TAplus - TAminus)/(2*epsilon))  # derivative of [TA] with respect to changes in the dissociation constants (Ks) times the derivative of the dissociation constants with respect to salinity S
          })
  }


# PRIVATE function:
# calculates the derivative of [TA] with respect to changes in the dissociation constants (Ks) times the derivative of the dissociation constants with respect to temperature T
dTAdKdKdT <- function(ae)                     # object of class aquaenv
  {
    with (ae,
          {
            epsilon <- Tc * Technicals$epsilon_fraction

            TAplus <- c()
            TAminus <- c()
           
            for (s in 1:length(S))
              {
                for (de in 1:length(d))
                  {
                    if (length(pH)     > 1) {i <- max(s,de)} else {i <- 1}
                    if (length(SumCO2) > 1) {x <- max(s,de)} else {x <- 1}
                    TAplus  <- c(TAplus, aquaenv(Tc=(Tc+epsilon), S=S[[s]], d=d[[de]],
                                                 SumCO2=SumCO2[[x]], SumNH4=SumNH4, SumH2S=SumH2S, SumH3PO4=SumH3PO4, SumSiOH4=SumSiOH4, SumHNO3=SumHNO3, SumHNO2=SumHNO2, 
                                                 SumBOH3=SumBOH3[[s]], SumH2SO4=SumH2SO4[[s]], SumHF=SumHF[[s]],
                                                 TA=NULL, pH=pH[[i]], pCO2=NULL, CO2=NULL, speciation=FALSE, dsa=FALSE, ae=NULL, from.data.frame=FALSE,  revelle=FALSE)$TA)
                    TAminus <- c(TAminus, aquaenv(Tc=(Tc-epsilon), S=S[[s]], d=d[[de]],
                                                  SumCO2=SumCO2[[x]], SumNH4=SumNH4, SumH2S=SumH2S, SumH3PO4=SumH3PO4, SumSiOH4=SumSiOH4, SumHNO3=SumHNO3, SumHNO2=SumHNO2, 
                                                  SumBOH3=SumBOH3[[s]], SumH2SO4=SumH2SO4[[s]], SumHF=SumHF[[s]],
                                                  TA=NULL, pH=pH[[i]], pCO2=NULL, CO2=NULL, speciation=FALSE, dsa=FALSE, ae=NULL, from.data.frame=FALSE,  revelle=FALSE)$TA)
                  }
              }
            return ((TAplus - TAminus)/(2*epsilon)) # derivative of [TA] with respect to changes in the dissociation constants (Ks) times the derivative of the dissociation constants with respect to temperature T
          })
  }


# PRIVATE function:
# calculates the derivative of [TA] with respect to changes in the dissociation constants (Ks) times the derivative of the dissociation constants with respect to depth d
dTAdKdKdd <- function(ae)                     # object of class aquaenv
  {
    with (ae,
          {
            epsilon <- d * Technicals$epsilon_fraction

            TAplus <- c()
            TAminus <- c()

            for (s in 1:length(S))
              {
                for (de in 1:length(d))
                  {
                    if (length(pH)     > 1) {i <- max(s,de)} else {i <- 1}
                    if (length(SumCO2) > 1) {x <- max(s,de)} else {x <- 1}
                    TAplus  <- c(TAplus, aquaenv(Tc=Tc, S=S[[s]], d=(d[[de]]+epsilon[[de]]),
                                                 SumCO2=SumCO2[[x]], SumNH4=SumNH4, SumH2S=SumH2S, SumH3PO4=SumH3PO4, SumSiOH4=SumSiOH4, SumHNO3=SumHNO3, SumHNO2=SumHNO2, 
                                                 SumBOH3=SumBOH3[[s]], SumH2SO4=SumH2SO4[[s]], SumHF=SumHF[[s]],
                                                 TA=NULL, pH=pH[[i]], pCO2=NULL, CO2=NULL, speciation=FALSE, dsa=FALSE, ae=NULL, from.data.frame=FALSE,  revelle=FALSE)$TA)
                    TAminus <- c(TAminus, aquaenv(Tc=Tc, S=S[[s]], d=(d[[de]]-epsilon[[de]]),
                                                  SumCO2=SumCO2[[x]], SumNH4=SumNH4, SumH2S=SumH2S, SumH3PO4=SumH3PO4, SumSiOH4=SumSiOH4, SumHNO3=SumHNO3, SumHNO2=SumHNO2, 
                                                  SumBOH3=SumBOH3[[s]], SumH2SO4=SumH2SO4[[s]], SumHF=SumHF[[s]],
                                                  TA=NULL, pH=pH[[i]], pCO2=NULL, CO2=NULL, speciation=FALSE, dsa=FALSE, ae=NULL, from.data.frame=FALSE,  revelle=FALSE)$TA)
                  }
              }
            return ((TAplus - TAminus)/(2*epsilon))  # derivative of [TA] with respect to changes in the dissociation constants (Ks) times the derivative of the dissociation constants with respect to depth d
          })
  }


# PRIVATE function:
# calculates the derivative of [TA] with respect to changes in the dissociation constants (Ks) times the derivative of the dissociation constants with respect to the total sulfate concentration (influence via scale conversion)
dTAdKdKdSumH2SO4 <- function(ae)              # object of class aquaenv
  {
    with (ae,
          {
            epsilon <- SumH2SO4 * Technicals$epsilon_fraction

            TAplus <- c()
            TAminus <- c()
           
            for (s in 1:length(S))
              {
                for (de in 1:length(d))
                  {
                    if (length(pH)     > 1) {i <- max(s,de)} else {i <- 1}
                    if (length(SumCO2) > 1) {x <- max(s,de)} else {x <- 1}
                    TAplus  <- c(TAplus, aquaenv(Tc=Tc, S=S[[s]], d=d[[de]],
                                                 SumCO2=SumCO2[[x]], SumNH4=SumNH4, SumH2S=SumH2S, SumH3PO4=SumH3PO4, SumSiOH4=SumSiOH4, SumHNO3=SumHNO3, SumHNO2=SumHNO2, 
                                                 SumBOH3=SumBOH3[[s]], SumH2SO4=SumH2SO4[[s]], SumHF=SumHF[[s]],
                                                 TA=NULL, pH=pH[[i]], pCO2=NULL, CO2=NULL, speciation=FALSE, dsa=FALSE, ae=NULL, from.data.frame=FALSE, 
                                                 SumH2SO4_Koffset=epsilon[[s]], revelle=FALSE)$TA)
                    TAminus <- c(TAminus, aquaenv(Tc=Tc, S=S[[s]], d=d[[de]],
                                                  SumCO2=SumCO2[[x]], SumNH4=SumNH4, SumH2S=SumH2S, SumH3PO4=SumH3PO4, SumSiOH4=SumSiOH4, SumHNO3=SumHNO3, SumHNO2=SumHNO2, 
                                                  SumBOH3=SumBOH3[[s]], SumH2SO4=SumH2SO4[[s]], SumHF=SumHF[[s]],
                                                  TA=NULL, pH=pH[[i]], pCO2=NULL, CO2=NULL, speciation=FALSE, dsa=FALSE, ae=NULL, from.data.frame=FALSE, 
                                                  SumH2SO4_Koffset=-epsilon[[s]], revelle=FALSE)$TA)
                  }
              }
            return ((TAplus - TAminus)/(2*epsilon))  # derivative of [TA] with respect to changes in the dissociation constants (Ks) times the derivative of the dissociation constants with respect to the total sulfate concentration (influence via scale conversion)
          })
  }


# PRIVATE function:
# calculates the derivative of [TA] with respect to changes in the dissociation constants (Ks) times the derivative of the dissociation constants with respect to the total fluoride concentration (influence via scale conversion)
dTAdKdKdSumHF <- function(ae)                 # object of class aquaenv
  {
    with (ae,
          {
            epsilon <- SumHF * Technicals$epsilon_fraction

            TAplus <- c()
            TAminus <- c()

            for (s in 1:length(S))
              {
                for (de in 1:length(d))
                  {
                    if (length(pH)     > 1) {i <- max(s,de)} else {i <- 1}
                    if (length(SumCO2) > 1) {x <- max(s,de)} else {x <- 1}
                    TAplus  <- c(TAplus, aquaenv(Tc=Tc, S=S[[s]], d=d[[de]],
                                                 SumCO2=SumCO2[[x]], SumNH4=SumNH4, SumH2S=SumH2S, SumH3PO4=SumH3PO4, SumSiOH4=SumSiOH4, SumHNO3=SumHNO3, SumHNO2=SumHNO2, 
                                                 SumBOH3=SumBOH3[[s]], SumH2SO4=SumH2SO4[[s]], SumHF=SumHF[[s]],
                                                 TA=NULL, pH=pH[[i]], pCO2=NULL, CO2=NULL, speciation=FALSE, dsa=FALSE, ae=NULL, from.data.frame=FALSE, 
                                                 SumHF_Koffset=epsilon[[s]], revelle=FALSE)$TA)
                    TAminus <- c(TAminus, aquaenv(Tc=Tc, S=S[[s]], d=d[[de]],
                                                  SumCO2=SumCO2[[x]], SumNH4=SumNH4, SumH2S=SumH2S, SumH3PO4=SumH3PO4, SumSiOH4=SumSiOH4, SumHNO3=SumHNO3, SumHNO2=SumHNO2, 
                                                  SumBOH3=SumBOH3[[s]], SumH2SO4=SumH2SO4[[s]], SumHF=SumHF[[s]],
                                                  TA=NULL, pH=pH[[i]], pCO2=NULL, CO2=NULL, speciation=FALSE, dsa=FALSE, ae=NULL, from.data.frame=FALSE, 
                                                  SumHF_Koffset=-epsilon[[s]], revelle=FALSE)$TA)
                  }
              }
            return ((TAplus - TAminus)/(2*epsilon))  # derivative of [TA] with respect to changes in the dissociation constants (Ks) times the derivative of the dissociation constants with respect to the total fluoride concentration (influence via scale conversion)
          })
  }


# PRIVATE function:
# calculates the revelle factor
revelle <- function(ae)                       # object of class aquaenv
  {
    CO2_0 <- ae$CO2
    SumCO2_0 <- ae$SumCO2  
    
    dSumCO2 <- SumCO2_0*Technicals$revelle_fraction
    dCO2 <- c()
    CO2new <- c()
    
    for (s in 1:length(ae$S))
      {
        for (de in 1:length(ae$d))
          {
            for (t in 1:length(ae$Tc))
              {
                with (ae,
                      {
                        if (length(TA)     > 1) {i <- max(s,de,t)} else {i <- 1}
                        if (length(SumCO2) > 1) {x <- max(s,de,t)} else {x <- 1}
                        CO2new  <<- c(CO2new, aquaenv(Tc=Tc[[t]], S=S[[s]], d=d[[de]],
                                                      SumCO2=(SumCO2[[x]]+dSumCO2[[x]]), SumNH4=SumNH4, SumH2S=SumH2S, SumH3PO4=SumH3PO4, SumSiOH4=SumSiOH4, SumHNO3=SumHNO3, SumHNO2=SumHNO2, 
                                                      SumBOH3=SumBOH3[[s]], SumH2SO4=SumH2SO4[[s]], SumHF=SumHF[[s]],
                                                      TA=TA[[i]], pH=NULL, pCO2=NULL, CO2=NULL, speciation=TRUE, dsa=FALSE, ae=NULL, from.data.frame=FALSE,  revelle=FALSE)$CO2)
                      })
              }
          }
      }
    dCO2 <- CO2new - CO2_0
    
    return((dCO2/dSumCO2) * (SumCO2_0/CO2_0)) # the revelle factor
  }
