#' Calculates the TAC based on the Harvest Strategy 1 proposed at the MSE Workshop held in Vancouver, Canada, on Oct. 17-19, 2017
#' 
#' This HCR has the following specifications
#' -computes a TAC
#' -no rebuilding plan, TAC = 0 if SSBcur<SSBlim
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
#' @param cr is the catch ratio per fleet (by gear/country)
#' @param err is the implementation error per fleet  
 

#' @return A TAC in mt
#' @author Desiree Tommasi

HCR1_TAC_byfleet <- function(dat, yr, SSBtrs, SSBlim, Ftgt,cr, err){

  #extract the current SSB, the spwning stock biomass in the terminal year of the stock assessement
  #we need to remove the second and third columns as the names are repeated later in the data frame
  SSBcur = (dat[,-c(2,3)] %>% filter(Year==yr))$SPB
  
  #Extract the current total biomass
  Btot = (dat[,-c(2,3)] %>% filter(Year==yr))$Bio_Smry
  
  #compute the f multiplier given the LRP and the current female spawning biomass
  #The fmulti determines how large the exploitation rate relative to Ftgt should be
  
  if (SSBcur >= SSBtrs){
    fmulti = 1
  } else if (SSBcur > SSBlim) {
    fmulti = SSBcur/SSBtrs
  } else {
    fmulti=0
  }
  
  #Calculates the exploitation rate given the status of the stock
  Er = Ftgt*fmulti
  
  #Calculate the exploitation rate by fleet using the average catch ratios from 1999-2015
  Er_fleet = Er*cr

  #computes the TAC by fleet
  TAC_fleet = Er_fleet*Btot
  
  #adds an implementation error - here it is the same across fleets.
  #Note that when the catch is split back across fleet for input into the OM
  #the error will already be included
  #If the error differes by fleet, it should be included when the TAC is split across fleets
  TAC_err = TAC_fleet$x*err
  
  #Computes the overall TAC
  TAC = sum(TAC_err)
  
  TAC_dat = list(TAC=TAC, TAC_flt = TAC_err)
  
  return(TAC_dat)
}