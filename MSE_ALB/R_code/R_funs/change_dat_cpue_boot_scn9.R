#' Add to a Stock Synthesis data file new catch data
#'
#' This function changes an SS dat file to
#' 1) add more catch data
#' 2) changes the numbers of catch data
#' 3) changes the end year
#' 4) adds dummy CPUE data and error for each of the two NPALB indexes: 0.1 and 0.12
#' 5) adds dummy size comp data and effective sample size
#' 
#' It then writes a new data file into the working directory
#'
#' @param ss_file_in filename of original dat file to be modified, with full path or relative to working directory
#' @param ss_file_out filename for the new dat file with full path or relative to working directory
#' @param t_asmt how often assessments are run in years (e.g if every three years, 3)
#' @param nrep how many times the catch matrix has to be replicated (1 means it remains as is)
#' @param cdat_new set of new catch data, based on previous assessment time TAC
#' @return A modified dat file.
#' @author Desiree Tommasi

change_dat_cpue_boot_scn9 <- function(ss_file_in, ss_file_out, t_asmt, nrep, cdat_new,tstep){
  
  #read in data file from previous assessment period
  om_dat = SS_readdat(ss_file_in, version = "3.24")
  
  #create a new data file based on the old to be modified
  om_dat_new = om_dat
  
  #change end year to the end of next assessment period
  om_dat_new$endyr = om_dat$endyr + t_asmt
  
  #change the length of the catch record
  #Note that there are catch records for every season in a year, for NPALB seasons=4
  om_dat_new$N_catch = om_dat$N_catch + (t_asmt*4)
  
  #repeat catch data according to the assessment period
  cdat_new3 = do.call("rbind", replicate(nrep, cdat_new, simplify = FALSE))

  #there are some ghost vessels as part of F25 not reporting their catch
  #in terms of 2400 mt per year up to when ~50000 mt are reached in step 4
  if (tstep<4){
    f25c = 14400*(tstep-1)
    cdat_new3$F25[1:4]=cdat_new3$F25[1:4]+(f25c/4)+600
    cdat_new3$F25[5:8]=cdat_new3$F25[5:8]+(f25c/4)+(600*2)
    cdat_new3$F25[9:12]=cdat_new3$F25[9:12]+(f25c/4)+(600*3)
  } else {
    f25c = 50000
    cdat_new3$F25[1:4]=cdat_new3$F25[1:4]+(f25c/4)
    cdat_new3$F25[5:8]=cdat_new3$F25[5:8]+(f25c/4)
    cdat_new3$F25[9:12]=cdat_new3$F25[9:12]+(f25c/4)
  }
   
  #label future years
  ind = seq(1, (t_asmt*4), by = 4)
  
  for (j in 1:t_asmt){
    cdat_new3$year[ind[j]:(ind[j]+3)] = rep((om_dat$endyr+j),4) 
  }
  
  #change the catch data
  catch_new = rbind(om_dat$catch, cdat_new3[,2:32])
  om_dat_new$catch = catch_new
  
  #change the CPUE data
  #cpue 40 and 41 are the only CPUE indeces actually used in the OM model
  
  cpue_old = om_dat$CPUE
  cpue40 = cpue_old %>% filter(index==40)
  cpue41 = cpue_old %>% filter(index==41)
  
  #Add dummy data for the additional future years of this time step
  yearn = ((om_dat$endyr+1):(om_dat$endyr+3))
  
  cpue40n = cpue_old[(dim(cpue_old)[1]-2):dim(cpue_old)[1],]
  cpue40n$year = yearn
  cpue40n$seas = c(rep(1, t_asmt))
  cpue40n$index = rep(40, t_asmt)
  cpue40n$obs = rep((cdat_new$F9[1]/18), t_asmt)
  #Note that the SE used here was the average from the 2017 assessment. 
  #This can be increased/decreased to change the observation error
  cpue40n$se_log = rep(0.2, t_asmt) 
  
  cpue41n = cpue_old[(dim(cpue_old)[1]-2):dim(cpue_old)[1],]
  cpue41n$year = yearn
  cpue41n$seas = c(rep(1, t_asmt))
  cpue41n$index = rep(41, t_asmt)
  cpue41n$obs = rep((cdat_new$F1[1]/18), t_asmt)
  #Note that the SE used here was the average from the 2017 assessment. 
  #This can be increased/decreased to change the observation error
  cpue41n$se_log = rep(0.2, t_asmt)
  
  i4041 = which(cpue_old$index==40|cpue_old$index==41)
  cpue_new = rbind(cpue_old[-i4041,],cpue40, cpue40n, cpue41, cpue41n)
  
  om_dat_new$CPUE = cpue_new
  om_dat_new$N_cpue = dim(cpue_new)[1]
  
  #Create dummy length composition data
  #Note that the Nsamp taken was the maximum Nsamp specified in the 2017 stock assessment dat file for that fleet
  #So in thi scase the comps are more informative than for any average year
  #Nsamp can be changed to increase/decrease the observation error
  len_old = om_dat$lencomp
  
  len1n = len_old %>% filter(Yr== 2013& FltSvy == 1 |Yr== 2014& FltSvy == 1 |Yr== 2015 & FltSvy == 1)
  len1n$Yr = yearn
  len1n[,7:124] = 1
  len1n$Nsamp = rep(1050,3)
  
  len2n = len1n
  len2n$Seas = 2
  len2n$FltSvy = 2
  len2n$Nsamp = rep(400,3)
  
  len3n = len1n
  len3n$Seas = 3
  len3n$FltSvy = 3
  len3n$Nsamp = rep(50,3)
  
  len4n = len1n
  len4n$Seas = 4
  len4n$FltSvy = 4
  len4n$Nsamp = rep(422,3)
  
  len9n = len1n
  len9n$FltSvy = 9
  len9n$Nsamp = rep(302,3)
  
  len10n = len_old %>% filter(Yr== 2013& FltSvy == 10 |Yr== 2014& FltSvy == 10 |Yr== 2015 & FltSvy == 10)
  len10n$Yr = c(rep((om_dat$endyr+1),3),rep((om_dat$endyr+2),3),rep((om_dat$endyr+3),3))
  len10n[,7:124] = 1
  len10n$Nsamp = rep(184,3)
  
  len13n = len_old %>% filter(Yr== 2013& FltSvy == 13 |Yr== 2014& FltSvy == 13 |Yr== 2015 & FltSvy == 13)
  len13n$Yr = c(rep((om_dat$endyr+1),2),rep((om_dat$endyr+2),2),rep((om_dat$endyr+3),2))
  len13n[,7:124] = 1
  len13n$Nsamp = rep(37,3)
  
  len16n = len2n
  len16n$FltSvy = 16
  len16n$Nsamp = rep(300,3)
  
  len17n = len3n
  len17n$FltSvy = 17
  len17n$Nsamp = rep(272,3)
  
  len19n = len_old %>% filter(Yr== 2013& FltSvy == 19 |Yr== 2014& FltSvy == 19 |Yr== 2015 & FltSvy == 19)
  len19n = rbind(len19n, len19n[8,])
  len19n$Yr = c(rep((om_dat$endyr+1),4),rep((om_dat$endyr+2),4),rep((om_dat$endyr+3),4))
  len19n[,7:124] = 1
  len19n$Nsamp = rep(7,3)
  
  len20n = len19n
  len20n$FltSvy = 20
  len20n$Nsamp = rep(8,3)
  
  len21n = len13n
  len21n$FltSvy = 21
  len21n$Seas = rep(c(1,4),3)
  len21n$Nsamp = rep(113,3)
  
  len27n = len13n
  len27n$FltSvy = 27
  len27n$Seas = rep(c(3,4),3)
  len27n$FltSvy = len27n$FltSvy
  len27n$Nsamp = rep(37,3)
  
  len_new = rbind(len_old, len1n, len2n, len3n, len4n, len9n, len10n, len13n, len16n, len17n, len19n, len20n, len21n, len27n)
  
  om_dat_new$lencomp = len_new
  om_dat_new$N_lencomp = dim(len_new)[1]
  
  #Write a stock synthesis data file from the mse_dat list object
  SS_writedat(om_dat_new, ss_file_out, version = "3.24")
  
}
  