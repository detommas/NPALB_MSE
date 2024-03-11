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

change_dat_cpue_boot_tactae <- function(ss_file_in, ss_file_out, t_asmt, nrep, cdat_new){
  
  #read in data file from previous assessment period
  om_dat = SS_readdat(ss_file_in,version = "3.24")
  
  #create a new data file based on the old to be modified
  om_dat_new = om_dat
  
  #change end year to the end of next assessment period
  om_dat_new$endyr = om_dat$endyr + t_asmt
  
  #change the length of the catch record
  #Note that there are catch records for every season in a year, for NPALB seasons=4
  om_dat_new$N_catch = om_dat$N_catch + (t_asmt*4)
  
  #repeat catch data according to the assessment period
  cdat_new3 = do.call("rbind", replicate(nrep, cdat_new, simplify = FALSE))

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
  
  cpue40n = cpue_old[(dim(cpue_old)[1]-(t_asmt-1)):dim(cpue_old)[1],]
  for (j in 1:t_asmt){
    cpue40n$year[j] = om_dat$endyr+j
  }
  cpue40n$seas = c(rep(1, t_asmt))
  cpue40n$index = rep(40, t_asmt)
  cpue40n$obs = rep((cdat_new$F9[1]/18), t_asmt)
  
  #Note that the SE used here was the average from the 2017 assessment. 
  #This can be increased/decreased to change the observation error
  cpue40n$se_log = rep(0.2, t_asmt) 
  
  cpue41n = cpue_old[(dim(cpue_old)[1]-(t_asmt-1)):dim(cpue_old)[1],]
  for (j in 1:t_asmt){
    cpue41n$year[j] = om_dat$endyr+j
  }
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
  
  #generate labels for the years that will be added
  yearn=1:t_asmt
  for (j in 1:t_asmt){
    yearn[j] = om_dat$endyr+j
  }
  
  #label for older years to use as a dummy
  yearo = yearn-t_asmt
  
  len1n = len_old %>% filter(Yr %in% yearo & FltSvy == 1)
  len1n$Yr = yearn
  len1n[,7:124] = 1
  len1n$Nsamp = rep(1050,t_asmt)
  
  len2n = len1n
  len2n$Seas = 2
  len2n$FltSvy = 2
  len2n$Nsamp = rep(400,t_asmt)
  
  len3n = len1n
  len3n$Seas = 3
  len3n$FltSvy = 3
  len3n$Nsamp = rep(50,t_asmt)
  
  len4n = len1n
  len4n$Seas = 4
  len4n$FltSvy = 4
  len4n$Nsamp = rep(422,t_asmt)
  
  len9n = len1n
  len9n$FltSvy = 9
  len9n$Nsamp = rep(302,t_asmt)
  
  len10n = len_old %>% filter(Yr %in% yearo & FltSvy == 10)
  ct=1
  for (j in seq(1,(t_asmt*3),3)){
    len10n$Yr[j:(j+2)] = yearn[ct]
    ct=ct+1
  }
  len10n[,7:124] = 1
  len10n$Nsamp = rep(184,(t_asmt*3))
  
  len13n = len_old %>% filter(Yr %in% yearo & FltSvy == 13)
  ct=1
  for (j in seq(1,(t_asmt*2),2)){
    len13n$Yr[j:(j+1)] = yearn[ct]
    ct=ct+1
  }
  len13n[,7:124] = 1
  len13n$Nsamp = rep(37,(t_asmt*2))
  
  len16n = len2n
  len16n$FltSvy = 16
  len16n$Nsamp = rep(300,t_asmt)
  
  len17n = len3n
  len17n$FltSvy = 17
  len17n$Nsamp = rep(272,t_asmt)
  
  if (yearo<2016){
    len19n = len_old %>% filter(Yr %in% yearo & FltSvy == 19)
    len19n = rbind(len19n, len19n[1,])
  }else{
    len19n = len_old %>% filter(Yr %in% yearo & FltSvy == 19)
  }
  ct=1
  for (j in seq(1,(t_asmt*4),4)){
    len19n$Yr[j:(j+3)] = yearn[ct]
    ct=ct+1
  }
  len19n$Seas=rep(c(1:4),t_asmt)
  len19n[,7:124] = 1
  len19n$Nsamp = rep(7,t_asmt)
  
  len20n = len19n
  len20n$FltSvy = 20
  len20n$Nsamp = rep(8,t_asmt)
  
  len21n = len13n
  len21n$FltSvy = 21
  len21n$Seas = rep(c(1,4),t_asmt)
  len21n$Nsamp = rep(113,t_asmt)
  
  len27n = len13n
  len27n$FltSvy = 27
  len27n$Seas = rep(c(3,4),t_asmt)
  len27n$FltSvy = len27n$FltSvy
  len27n$Nsamp = rep(37,t_asmt)
  
  len_new = rbind(len_old, len1n, len2n, len3n, len4n, len9n, len10n, len13n, len16n, len17n, len19n, len20n, len21n, len27n)
  
  om_dat_new$lencomp = len_new
  om_dat_new$N_lencomp = dim(len_new)[1]
  
  #Write a stock synthesis data file from the mse_dat list object
  SS_writedat(om_dat_new, ss_file_out, version = "3.24")
  
}
  