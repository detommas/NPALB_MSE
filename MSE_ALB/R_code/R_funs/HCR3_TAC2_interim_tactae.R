#' Calculates the catch by fleet based on a specified harvest rate
#' 
#' Here the exploitation rate is the ratio of the total catch in weight over the total biomass
#' Note that there is no implementation error as that's included in tae, which is set every three years
#' @param dat specifies the data frame which contains the sprseries data extracted from the om
#' @param yr specifies the year for which to extract the current total biomass
#' @param tae specifies the tae that was set during the assessement, it applies to surface fleets and already has an implmentation error
#' @param tac speccifies the tac that was set during the assessement, it applies to longline fleets 
#' @param taci speccifies the tac that was set during the assessement, it applies to longline fleets and already has an implmentation error
#' @param cr is the catch ratio by fleet
#' @param err is the implementation error

#' @return A catch in mt overall and by fleet
#' @author Desiree Tommasi

HCR3_TAC2_interim_tactae <- function(dat, yr, tae,tac,taci, cr){
  
  #Extract the current total biomass
  #we need to remove the second and third columns as the names are repeated later in the data frame
  Btot = (dat[,-c(2,3)] %>% filter(Yr==yr))$Bio_Smry
  
  #Calculate the exploitation rate by fleet
  Eri_fleet = cr
  Eri_fleet[,2:30] = cr[,2:30]*tae
  
  #computes the catch by fleet resulting from the TAE, this applies only to fleets 16 to 18 and 27, the surface fleets
  TACi_fleet_hr = cr
  TACi_fleet_hr[,2:30] = (Eri_fleet[,2:30]*Btot)
  
  #the TAC applies to all other fleets
  TACi_fleet = cr
  TACi_fleet[,2:30] = cr[,2:30]*taci
  
  #Replace the surface fllets with the catch computed by the tae
  TACi_fleet$F16=TACi_fleet_hr$F16
  TACi_fleet$F17=TACi_fleet_hr$F17
  TACi_fleet$F18=TACi_fleet_hr$F18
  TACi_fleet$F27=TACi_fleet_hr$F27

  #Fleets 5-8, 11-12, and 14-15 have catch data actually inputted in numbers rather
  #than weight, so have to back transform to catch in numbers before input into SS
  
  #extract weight conversion for terminal year for fleets 5-8, 11-12, and 14-15
  #Note that there are four rows as each year has four seasons
  ba_conv_tyr = ba_conv[((dim(ba_conv)[1])-3): (dim(ba_conv)[1]),]
  
  #convert into numbers. Note that as catch weight is in mt
  #and numbers are in 1000s of fish we can simply multiply the catch in biomass by the conversion factor
  flt = c(5:8,11,12, 14, 15)
  
  for (f in 1:8){
    TACi_fleet[1:4,(flt[f]+1)]=TACi_fleet[1:4,(flt[f]+1)]*ba_conv_tyr[1:4,f]
  }
  
  #Note that when no fish were captured in the specified year, season, and fleet, 
  #the average weight is NA, resulting in NA catch
  #Set NA to 0
  for (f in 1:8){
    TACi_fleet[is.na(TACi_fleet[,(flt[f]+1)]),(flt[f]+1)]=0
  }
  
  
  TACi_dat = list(TACi=taci, TACp=tac, TAEi = tae, TACi_flt = TACi_fleet)
  
  return(TACi_dat)
}