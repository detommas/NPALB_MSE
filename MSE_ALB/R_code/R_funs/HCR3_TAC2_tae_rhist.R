#' Calculates the TAE based on the Harvest Strategy 3 proposed at the MSE Workshop held in Vancouver, Canada, on Oct. 17-19, 2017
#' 
#' This HCR has the following specifications
#' -computes a TAE
#' -no rebuilding plan, TAE = TAEmin if SSBcur<SSBlim
#' -management action occurs when either SSBcur<SSBthreshold or when SSBcur<SSBlim with a specified probability
#' -allocation of the TAE is based on the average 1999-2015 allocation
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
#' @param TAcmax is the maximum hisotrical catch not be exceeded by the TAC
#' @param plim is the required probability that SSB be > LRP
 

#' @return A TAC in mt
#' @author Desiree Tommasi

HCR3_TAC2_tae_rhist <- function(myreplist,dat, datom, yr, yrom, SSBtrs, SSBlim, Ftgt,TACmin,TACmax,plim,err,cr,pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep,hcrnum){

  #extract the current SSB, the spawning stock biomass in the terminal year of the stock assessement
  #we need to remove the second and third columns as the names are repeated later in the data frame
  #both from the EM
  SSBcur = (dat[,-c(2,3)] %>% filter(Yr==yr))$SSB
  
  #Extract the current total biomass from the OM
  Btotom = (datom[,-c(2,3)] %>% filter(Yr==yrom))$Bio_Smry
  
  #compute the minimum TAC
  tacm = (Ftgt*Btotom*SSBlim/SSBtrs)*TACmin
  
  #**************************************************************************************************************
  #Produce the input data for the Future Projection software
  
  d1 = get_naa(myreplist)    #Get n at age
  d2 = get_caa(myreplist)    #Get catch at age
  d4 = get_maa(myreplist)    #Get maturty at age
  syr = myreplist$startyr    #extract the start year
  dim1 = length(syr:yr)
  Mv=c(rep(d4$maa[1],dim1),rep(d4$maa[2],dim1),rep(d4$maa[3],dim1),rep(d4$maa[4],dim1*13),rep(d4$maa[17],dim1),rep(d4$maa[18],dim1),rep(d4$maa[19],dim1),rep(d4$maa[20],dim1*13))
  d1$naa$M=rep(Mv*0.25,4) #create a natural mortality vector the same dimensions as the numbers at age, have to change annual to season values so multiply by 0.25
  d3 = get_faa(d1$naa,d2,myreplist)    #Get f at age
  #Future Projection uses the average of the F in the three years preceding the current one
  tmp = d3  %>% filter(Yr>=(yr-3),Yr<=(yr-1)) %>% group_by(Gender,age) %>% summarise(f=mean(faa))
  fend = tmp$f
  rm(tmp)
  d5 = get_rec(myreplist)    #Get spawner recruitment relationship
  d6 = get_ac(myreplist)     #Get autocorrelation of recruits data
  #Average catch weight for the three years preceding the current one
  d7 = get_cat_b(myreplist)
  tmp = d7 %>% filter(Yr>=(yr-3),Yr<=(yr-1))  %>% summarise(catb=mean(Retain_Catch_B))
  catbend = tmp$catb
  rm(tmp)
  d8 = 0.4 #set cv of SSB
  
  #Produces a set of random initial conditions in terms of numbers at age based on the CV of SSB, here specified to 0.4
  int_n = list()
  tmp = rep(0,(d4$age+1)*2)
  maxage = (d4$age+1)*d4$gender
  for (i in 1:500) {
    for (j in 1:maxage) {
      tmp[j] = rnorm(1, d1$int_n[j], d1$int_n[j]*d8)
    }
    int_n[[i]] = tmp  
  }
  rm(tmp)
  
  #run future projection simulation*****************************************************************************
  for (i in 1:500) {
    
    d = list(int_n=int_n[[i]], #Set initial population number
             f=fend,      #Set constant Fishing mortality (define selectivity)
             catb=catbend,  #Set target catch weight
             maa=d4$maa,       #Set natural mortality at age
             waa=d4$waa,       #Set weight at age
             mat=d4$mat,       #Set maturity at age
             Rzero=d5$Rzero,   #Set R0
             SBzero=d5$SBzero, #Set SB0
             h=d5$h,           #Set Steepness
             sigmar=d5$sigmar, #Set sigma R
             rho=d6,           #Set autocorrelation parameter
             age=d4$age,       #Set maximum Age
             gender=d4$gender, #Set gender type 1:one gender, 2:two gender
             imax=1000,        #Set iteration number
             tmax=10,          #Set projectoin year
             recfun=2,         #Set type of recruitment uncertainty 1:normal 2:autocorrelation
             manage=1          #Set management scenario 1:Constant F, 2:Constant catch
    )
    
    set.seed(66)
    #assign(paste("res", i, sep=""), ssfcpp(d))
    res_list = ssfcpp(d)
    save(res_list, file=paste("res", i,"." ,"RData", sep=""))
  }
  
  #calculate probability that SSB is greater than the LRP in any year of the future projection
  pblim = length(which(res_list$SB>=SSBlim))/length(res_list$SB)
  rm(res_list)
  #**************************************************************************************************************
  #compute the TAC given the LRP and the current female spawning biomass
  #and the result of the future projection
  
  if (SSBcur >= SSBtrs){
    
    #F is set to a random set of historical Fs
    #run the estimation model with the same params but the new ftgt from random historical
    EM_fun_newftgt(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep,hcrnum)
    #read EM2 output file
    em2_dir = paste(pdir, hs, hcr, scn, itr, "/",tstep,"/EM2/", sep = "")
    em2_out = SS_output(em2_dir, covar = FALSE, ncols = 250)
    #Extract the fishing intensity (as exploitation rate) that results in the SPR target reference point (F target) specified in the forecast file
    em2_ftgt = (em2_out$derived_quants %>% filter(Label == "Fstd_SPRtgt"))$Value
    
    #TAC is computed as the random historical exploitation rate*current biomass
    TACt = em2_ftgt*Btotom
    
  } else if (pblim > plim) {
    
    #compute overall TAC
    TACt = tacm +((Ftgt*Btotom-tacm)/(SSBtrs-SSBlim))*(SSBcur-SSBlim)

} else {
    
    #compute overall TAC
    TACt = tacm

  }
  
  if (TACt>TACmax) {TAC = TACmax} else {TAC = TACt}
  
  if (TAC<tacm) {TAC = tacm} 
  
  TAE = TAC/Btotom
  TACi = TAC*err
  TAEi = TACi/Btotom
  
  #Calculate the exploitation rate by fleet
  Eri_fleet = cr
  Eri_fleet[,2:30] = cr[,2:30]*TAEi 
  
  #computes the catch by fleet resulting from the TAE
  #We multiply by the biomass from the OM
  TACi_fleet_hr = cr
  TACi_fleet_hr[,2:30] = (Eri_fleet[,2:30]*Btotom)
  
  #Fleets 5-8, 11-12, and 14-15 have catch data actually inputted in numbers rather
  #than weight, so have to back transform to catch in numbers before input into SS
  
  #extract weight conversion for terminal year for fleets 5-8, 11-12, and 14-15
  #Note that there are four rows as each year has four seasons
  ba_conv_tyr = ba_conv[((dim(ba_conv)[1])-3): (dim(ba_conv)[1]),]
  
  #convert into numbers. Note that as catch weight is in mt
  #and numbers are in 1000s of fish we can simply multiply the catch in biomass by the conversion factor
  flt = c(5:8,11,12, 14, 15)
  
  for (f in 1:8){
    TACi_fleet_hr[1:4,(flt[f]+1)]=TACi_fleet_hr[1:4,(flt[f]+1)]*ba_conv_tyr[1:4,f]
  }
  
  #Note that when no fish were captured in the specified year, season, and fleet, 
  #the average weight is NA, resulting in NA catch
  #Set NA to 0
  for (f in 1:8){
    TACi_fleet_hr[is.na(TACi_fleet_hr[,(flt[f]+1)]),(flt[f]+1)]=0
  }
  
  
  TACi_dat = list(TACi=TACi, TACp=TAC, TAEi = TAEi, TACi_flt = TACi_fleet_hr)
  
  return(TACi_dat)
  
}