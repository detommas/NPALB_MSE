#' Computes a biomass reference point for North Pacific albacore tuna
#' given an SS model output file 
#'
#' @param ssoutput An SS Report.sso file already read into R using the function SS_ouput in the package r4ss
#' @param iyear initial year over which to average the dynamic spawning biomass with no fishing
#' It is dynamic as the maximum spawning biomass (i.e. with no fishing) changes
#' given the current biology
#' @param fyear final year over which to average the dynamic spawning biomass with no fishing
#' @param fraction specifies the fraction of maximum spawning biomass to be preserved
#' @return a biomass reference point
#' @author Desiree Tommasi and H. Ijima

brp_fun <- function(ssoutput, iyear, fyear, fraction){
  
  #extract dynamic bzero data
  Dynamic_Bzero = ssoutput$Dynamic_Bzero
  
  #Calculation of reference point given the percent SPR
  tmp = as.numeric(Dynamic_Bzero %>% filter(Yr>=iyear&Yr<=fyear) %>% summarise(mean(SSB_nofishing)))
  BRP = fraction*tmp
  
  return(BRP)
}