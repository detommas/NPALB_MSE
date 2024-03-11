#' Runs the NPALB MSE framework for the specified number of iterations
#'
#' Note that this was created to be run using the wrapper function MSE_prll.R
#' @param hsnum number characterizing the harvest strategy being run
#' @param hcrnum number characterizing the harvest control rule being run
#' @param scnnum number characterizing the uncertainity scenario being run
#' @param itr the iteration that one wants to run
#' @param Bthr the fraction of unifished biomass the threshold reference point refers to
#' @param FBtrh the fraction of Btrh to consider for the threshold reference point
#' @param Blim the fraction of unfished biomass the limit reference point refers to
#' @param Tmin specifies the fraction of the minimum catch at the LRP
#' @param pl specifies the level of probability of SSB>LRP required by the hcr
#' Note that the F target is specified in the forecast file. The fishing intensity is measured as the spawning potential ratio (SPR). 
#' SPR is the calculated as the ratio of the equilibrium reproductive output per recruit that would occur 
#' with the current year's F intensities and biology, to the equilibrium reproductive output per recruit 
#' that would occur with the current year's biology and no fishing.
#' @return a data frame of output for performance statistics
#' @author D.Tommasi

NPALB_MSE2_hs3_tvry55 = function(hsnum,hcrnum,scnnum,itr, rdev1, Bthr, Blim, Tmin, pl) { 

#specify the path for the harvest strategy that is being run
hs = paste(hsnum, "/", sep = "")
hsw = paste(hsnum, "\\", sep = "")

#specify the path for the harvest control rule (i.e. combination of reference points) within that hs that is being run
hcr = paste(hcrnum, "/", sep = "")
hcrw = paste(hcrnum, "\\", sep = "")

#specify the path for the scenario (i.e. om model type) that hs that is being run
scn = paste(scnnum, "/", sep = "")
scnw = paste(scnnum, "\\", sep = "")

#Specify parent directories path 
pdir = "J:/Desiree/MSE_2/MSE_ALB/"
pwin = "J:\\Desiree\\MSE_2\\MSE_ALB\\"

#Specify the path of conditioned initial OM
sdir = "J:/Desiree/MSE_2/MSE_ALB/Condition/"

#Specify vectors where to save output (output is from OM unless otherwise specified) for the future simulation years
Rdat = 1:30 # current recruits
SPBdat = 1:30 # current spawning biomass
Btot = 1:30 # current total biomass
Tdat = 1:30 # current total catch
Cdat = matrix(data = NA, nrow = 30, ncol = 30) #current catch by fleet
TACdt = 1:30 # current total TAC
Ddat = 1:30 # current depletion
SPRdat = 1:30 # current fishing intensity (1-SPR)
B0dat = 1:30 # current dynamic SSB0 from em
B0dat_om = 1:30 # current dynamic SSB0 from om
Ftgt_om = 1:30 # F target (exploitation intensity leading to the SPR specified in the forcast file) from om
Ftgt_em = 1:30 # F target (exploitation intensity leading to the SPR specified in the forcast file) from em
TACi = 1:30 #TAC plus implementation error
R_em = 1:30 #Recruits from em
SPB_em = 1:30 #spawning stock biomass from EM
D_em = 1:30 #Depletion from EM
SPR_em = 1:30 #SPR from EM
Btot_em = 1:30 #total biomass from EM
C_em = 1:30 #total catch from EM

#set working directory 
setwd(paste(pdir,hs, hcr, scn, sep = ""))

#create directory for each iteration (i.e. different recruitment and time varying selectivity errors)
cmddir = paste("mkdir", itr)
shell(cmd = cmddir)

#**************************************************************************************
#Generate recruitment deviations

#set working directory to base iteration
setwd(paste(pdir, hs, hcr, scn, itr, sep = ""))

#Create a folder where to store deviations for each MSE iteration (i.e. 30 year simulation), and the implementation error
shell(cmd = "mkdir Rec_dev")

rec_devs = recdevs_alb_mse_rho(itr, 30, rdev1, 0.42)

write.table(rec_devs, paste(pdir, hs, hcr, scn, itr,"/Rec_dev/rec_devs.txt", sep = ""))

#create the implementation error per fleet
imp_e = implem_bierr(itr, n=30, sigma=0.05)

write.table(imp_e, paste(pdir, hs, hcr, scn, itr,"/Rec_dev/imp_e.txt", sep = ""))

#create the deviations for the time varying selectivity
sel_devs = seldevs_alb(itr, n=30, na=4)

write.table(sel_devs, paste(pdir, hs, hcr, scn, itr,"/Rec_dev/sel_devs.txt", sep = ""))

#**************************************************************************************

for (tstep in 1:10){
  
  #create directory for new time step where the new dat file will be saved
  setwd(paste(pdir,hs, hcr, scn, itr, sep = ""))
  cmddir = paste("mkdir", tstep)
  shell(cmd = cmddir)
  
  #*************************************************************************************
  #Step 1: Modify original dat file to include the TAC as catch for the next three years
  
  #*************************************************************************************
  if (tstep == 1) {
     TACdat = 95000
     TACmat = 95000
     TACdt[asmt_t[tstep]:(asmt_t[tstep]+2)] = rep(TACmat,3)
  } else {
    TAC_file = paste(pdir, hs, hcr, scn, itr,"/TAC",tstep,".txt", sep = "")
    TACmat = read.table(TAC_file)
    #add implmeentation error
    TACdat = TACmat$x*imp_e[asmt_t[tstep]]
    TACdt[asmt_t[tstep]:(asmt_t[tstep]+2)] = rep(TACmat$x,3)
  }
  
  TACi[asmt_t[tstep]:(asmt_t[tstep]+2)] = rep(TACdat,3)
  
  #Generate catch by fleet and season using the catch ratios (cr_hcr) produced by the
  #catch_ratios.R code and the TAC
  new_cdat = cr_hcr
  new_cdat[,2:30] = cr_hcr[,2:30]*TACdat
  
  #Fleets 5-8, 11-12, and 14-15 have catch data actually inputted in numbers rather
  #than weight, so have to back transform to catch in numbers before input into SS
  
  #extract weight conversion for terminal year for fleets 5-8, 11-12, and 14-15
  #Note that there are four rows as each year has four seasons
  ba_conv_tyr = ba_conv[((dim(ba_conv)[1])-3): (dim(ba_conv)[1]),]
  
  #convert into numbers. Note that as catch weight is in mt
  #and numbers are in 1000s of fish we can simply multiply the catch in biomass by the conversion factor
  flt = c(5:8,11,12, 14, 15)
  
  for (f in 1:8){
    new_cdat[1:4,(flt[f]+1)]=new_cdat[1:4,(flt[f]+1)]*ba_conv_tyr[1:4,f]
  }
  
  #Note that when no fish were captured in the specified year, season, and fleet, 
  #the average weight is NA, resulting in NA catch
  #Set NA to 0
  for (f in 1:8){
    new_cdat[is.na(new_cdat[,(flt[f]+1)]),(flt[f]+1)]=0
  }
  
  #***************************************************************************
  #Step 2: Estimate data with error and no error as input to EM and OM via SS Bootstrap routine
  OMBoot_fun_tvry(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep, tasmt, new_cdat, rec_devs, sel_devs)
  
  #****************************************************************************
  #Step 3: run operating model
  OM_fun_tvry(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep, tasmt, new_cdat, rec_devs, sel_devs)
  
  #****************************************************************************
  #Step 4: Run the estimation model
  EM_fun_nohess(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep)
  
  #****************************************************************************
  #Step 5: Compute TAC using EM model output
  
  #read EM output file
  out_dir = paste(pdir, hs, hcr, scn, itr, "/",tstep,"/EM/", sep = "")
  em_out = SS_output(out_dir, covar = FALSE, ncols = 250)
  
  yr_end = em_out$endyr
  
  #Compute the threshold and limit biomass reference points and specify the years the dynmaic SSB is averaged over
  ssb_thr = brp_fun(ssoutput=em_out, iyear=(2012+3*tstep), fyear=(2014+3*tstep), fraction=Bthr)
  ssb_lim = brp_fun(ssoutput=em_out, iyear=(2012+3*tstep), fyear=(2014+3*tstep), fraction=Blim)
  
  #Extract SPR series data, which includes spawning stock biomass
  SPRmat = em_out$sprseries
  
  #Extract the fishing intensity (as exploitation rate) that results in the SPR target reference point (F target) specified in the forecast file
  em_ftgt = (em_out$derived_quants %>% filter(LABEL == "Fstd_SPRtgt"))$Value
  
  #Generate TAC based on current harvest control rule
  TAC_mat = HCR3_TAC2(myreplist = em_out, dat = SPRmat, yr=yr_end, SSBtrs=ssb_thr, SSBlim=ssb_lim, Ftgt=em_ftgt, TACmin=Tmin, TACmax=120000, plim = pl)

  #Save the TAC
  file_tac = paste(pdir, hs, hcr, scn, itr,"/TAC",(tstep+1),".txt", sep = "")
  write.table(TAC_mat, file_tac)
  
  #Extract information from EM relative to performance metrics and EM model assessment
  DBzero_em = em_out$Dynamic_Bzero
  DBzero_lat_em = DBzero_em %>% filter(Yr== (yr_end-2) |Yr== (yr_end-1) |Yr== yr_end)
  B0dat[asmt_t[tstep]:(asmt_t[tstep]+2)] = DBzero_lat_em$SPB_nofishing
  Ftgt_em [asmt_t[tstep]:(asmt_t[tstep]+2)] = rep(em_ftgt,3)
  R_em[asmt_t[tstep]:(asmt_t[tstep]+2)] = SPRmat$Recruits[(dim(SPRmat)[1]-2):dim(SPRmat)[1]]
  SPB_em[asmt_t[tstep]:(asmt_t[tstep]+2)] = SPRmat$SPB[(dim(SPRmat)[1]-2):dim(SPRmat)[1]]
  D_em[asmt_t[tstep]:(asmt_t[tstep]+2)] = SPRmat$Deplete[(dim(SPRmat)[1]-2):dim(SPRmat)[1]]
  SPR_em[asmt_t[tstep]:(asmt_t[tstep]+2)] = SPRmat$SPR[(dim(SPRmat)[1]-2):dim(SPRmat)[1]]
  Btot_em[asmt_t[tstep]:(asmt_t[tstep]+2)] = SPRmat[(dim(SPRmat)[1]-2):dim(SPRmat)[1],15] #Bio_Smry
  C_em[asmt_t[tstep]:(asmt_t[tstep]+2)] = SPRmat$Retain_Catch[(dim(SPRmat)[1]-2):dim(SPRmat)[1]]
  
}

#****************************************************************************
#Step 6: Run the OM on the last time step with the data with no error

Path = paste(pdir, hs, hcr, scn, itr, "/", tstep,"/OM/", sep="")
filename_om  <-paste(Path,"ssnohess.bat",sep="")
batchtext_om = paste(pwin,"SS_model\\ss3.exe -nohess -cbs 500000000",sep="")
writeLines(batchtext_om,filename_om)

setwd(Path)
command_run_om="ssnohess.bat"
shell(cmd= command_run_om)

#****************************************************************************
#Step 7: Extract from the OM data relevant to calculation of performance metrics

out_dir = paste(pdir, hs, hcr, scn, itr,"/",tstep, "/OM/", sep="")
om_out = SS_output(out_dir, covar = FALSE, ncols = 250)

#Extract spr series quantities from the SS output
derquant = om_out$sprseries

#start of simulation
st=length(1993:2015)+1
#extract from spr series quantities relevant to performance metrics
Rdat[1:(tasmt*tstep)] = derquant$Recruits[st:length(derquant$Recruits)]
SPBdat[1:(tasmt*tstep)] = derquant$SPB[st:length(derquant$SPB)]
Ddat[1:(tasmt*tstep)] = derquant$Deplete[st:length(derquant$Deplete)]
SPRdat[1:(tasmt*tstep)] = derquant$SPR[st:length(derquant$SPR)]
Btot[1:(tasmt*tstep)] = derquant[st:length(derquant$SPR),15] #Bio_Smry
Tdat[1:(tasmt*tstep)] = derquant$Retain_Catch[st:length(derquant$SPR)]
Ftgt_om [1:(tasmt*tstep)] = rep(om_out$derived_quants$Value[which(om_out$derived_quants$LABEL == "Fstd_SPRtgt")],(tasmt*tstep))

#extract the catch by fleet and season and year 
catch = om_out$catage
catch[,27] = rowSums(catch[,11:26])

#extract the terminal year
yr_end = om_out$endyr

#select the catch from 2015 onwards
catch_lat = catch %>% filter(Yr> 2015) 

#sum catch across seasons
catch_sum = as.data.frame(catch_lat %>% group_by(Yr,Fleet) %>% summarise(cat=sum(V27)))

#set to wide format to match other data, so that year is in rows and column is the catch by fishery
catch_wide <- dcast(catch_sum, Yr ~ Fleet, value.var="cat")

Cdat[1:(tasmt*tstep),] = as.matrix(catch_wide)

#extract dynamic bzero data
DBzero = om_out$Dynamic_Bzero
DBzero_lat = DBzero %>% filter(Yr>2015)
B0dat_om[1:(tasmt*tstep)] = DBzero_lat$SPB_nofishing

#Combine all output into a list
outmat = data.frame(Year = 2016:2045, R = Rdat, Rem = R_em, SSB = SPBdat, SSBem = SPB_em, Depletion = Ddat, Dem = D_em, SPR = SPRdat, SPRem = SPR_em, Catch = Tdat, Cem = C_em, TAC = as.numeric(TACdt), TACi = as.numeric(TACi), LRPem = B0dat, LRPom = B0dat_om, Ftgtom = Ftgt_om, Ftgtem = Ftgt_em, F1 = Cdat[,2], F10 = Cdat[,11],F11 = Cdat[,12], F12 = Cdat[,13], F13 = Cdat[,14], F14 = Cdat[,15], F15 = Cdat[,16], F16 = Cdat[,17], F17 = Cdat[,18], F18 = Cdat[,19], F19 = Cdat[,20], F2 = Cdat[,3], F20 = Cdat[,21], F21 = Cdat[,22], F22 = Cdat[,23],F23 = Cdat[,24], F24 = Cdat[,25], F25 = Cdat[,26], F26 = Cdat[, 27], F27 = Cdat[, 28], F28 = Cdat[, 29], F29 = Cdat[, 30], F3 = Cdat[,4], F4 = Cdat[, 5], F5 = Cdat[, 6], F6 = Cdat[, 7], F7=Cdat[, 8], F8 = Cdat[, 9], F9 = Cdat[, 10])
outlist= list(outmat=outmat)

#save output to file
write.table(outlist, paste(pdir,hs, hcr,scn, itr,"/outlist.txt", sep =""))

return(outmat)
}