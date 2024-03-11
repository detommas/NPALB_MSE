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
#' @param tae the total allowable effort allowed in the previous assessment
#' @param cr is the catch ratio by gear/country
#' @param err is the implementation error per fleet  
#' which is the ratio of the total annual catch to the biomass on Jan. 1. 

#' @return A TAC in mt
#' @author Desiree Tommasi

HCR3_TAC_byfleet_interim <- function(dat, yr, tae, cr, err){

  #extract the current SSB, the spwning stock biomass in the terminal year of the stock assessement
  #we need to remove the second and third columns as the names are repeated later in the data frame
  SSBcur = (dat[,-c(2,3)] %>% filter(Year==yr))$SPB
  
  #Extract the current total biomass
  Btot = (dat[,-c(2,3)] %>% filter(Year==yr))$Bio_Smry
  
  #Calculates the exploitation rate given the status of the stock
  Eri = tae
  
  #Calculate the exploitation rate by fleet
  Eri_fleet = Eri*cr

  #computes the TAC by fleet
  TACi_fleet = Eri_fleet*Btot
  
  #adds an implementation error
  TACi_err = TACi_fleet$x*err
  
  #Computes the overall TAC
  TACi = sum(TACi_err)
  
  TACi_dat = list(TACi=TACi, TACi_flt = TACi_err)
  
  return(TACi_dat)
}