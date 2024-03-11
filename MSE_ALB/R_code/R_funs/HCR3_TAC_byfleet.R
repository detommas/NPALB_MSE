#' Calculates the TAC based on the Harvest Strategy 3 proposed at the MSE Workshop held in Vancouver, Canada, on Oct. 17-19, 2017
#' 
#' This HCR has the following specifications
#' -computes a TAC
#' -no rebuilding plan, TAC = TACmin if SSBcur<SSBlim
#' -management action occurs when either SSBcur<SSBthreshold or when SSBcur<SSBlim with a 50% probability
#' -allocation of the TAC is based on the average 1999-2015 allocation
#' -HCR controls both the albacore targeting and non-targeting fleets
#' 
#' Here the exploitation rate is the ratio of the total catch in weight over the total biomass
#' @param dat specifies the data frame which contains the sprseries data extracted from the stock assessment output
#' @param yr specifies the year for which to extract the current total biomass
#' @param SSBtrs the treshold biomass reference point
#' @param SSBlim the limit biomass reference point
#' @param Ftgt is the fishing intensity that produces the specified SPR target. It is computed as an exploitation rate, 
#' which is the ratio of the total annual catch to the biomass on Jan. 1.
#' @param TACmin is the minimum TAC
#' @param cr is the catch ratio by gear/country
#' @param err is the implementation error per fleet  
 

#' @return A TAC in mt
#' @author Desiree Tommasi

HCR3_TAC_byfleet <- function(dat, yr, SSBtrs, SSBlim, Ftgt,TACmin, cr, err){

  #extract the current SSB, the spawning stock biomass in the terminal year of the stock assessement
  #we need to remove the second and third columns as the names are repeated later in the data frame
  SSBcur = (dat[,-c(2,3)] %>% filter(Year==yr))$SPB
  
  #Extract the current total biomass
  Btot = (dat[,-c(2,3)] %>% filter(Year==yr))$Bio_Smry
  
  #compute the f multiplier given the LRP and the current female spawning biomass
  #The fmulti determines how large the exploitation rate relative to Ftgt should be
  
  if (SSBcur >= SSBtrs){
    #F is set to F target
    #An exploitation rate by fleet is calculated using the Ftgt and catch ratios
    Er_fleet = Ftgt*cr
    #computes the TAC by fleet
    TAC_fleet = Er_fleet*Btot
    
    #adds an implementation error
    TAC_err = TAC_fleet*err
    
    #Computes the overall TAC
    TAC = sum(TAC_err$x)
    
  } else if (SSBcur > SSBlim) {
    
    #compute overall TAC
    TACtmp = TACmin +((Ftgt*Btot-TACmin)/(SSBtrs-SSBlim))*(SSBcur-SSBlim)
    #TAC by fleet with implementation error
    TAC_fleet = TACtmp*cr
    TAC_err = TAC_fleet*err
    #Computes overall TAC with error
    TAC = sum(TAC_err$x)
    
  } else {
    
    #compute overall TAC
    TACtmp = TACmin 
    #TAC by fleet with implementation error
    TAC_fleet = TACtmp*cr
    TAC_err = TAC_fleet*err
    #Computes overall TAC with error
    TAC = sum(TAC_err$x)
    
  }
  
  TAC_dat = list(TAC=TAC, TAC_flt = TAC_err)
  
  return(TAC_dat)
}