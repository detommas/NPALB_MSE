#' Prepares the needed files to run the NPALB EM model in SS and run the model
#' 
#' @param pdir the parent directory path
#' @param sdir the path to directory of OM (initial)
#' @param hs the harvest strategy being run
#' @param hcr the hcr being run
#' @param scn the scenario being run
#' @param hsw the harvest strategy being run in Windows notation
#' @param hcrw the hcr being run in Winodws notation
#' @param scnw the scenario being run in Windows notation
#' @param pwin he parent directory path in wondows notation
#' @param itr iteration number
#' @param tstep time step of the OM
#' 
#' @author Desiree Tommasi

EM_fun_nohess <- function(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep){
 
#*****************************CHANGE DAT FILE*******************************************   
  # Enter new catch data given the TAC into the PAR file
  
  #create directory for the operating model
  setwd(paste(pdir, hs, hcr, scn, itr,"/", tstep, sep = ""))
  cmddir = "mkdir EM"
  shell(cmd = cmddir)
  
  #move to directory with Bootstrap run
  setwd(paste(pdir, hs, hcr, scn, itr,"/", tstep, "/Boot/", sep = ""))
  
  #read the new .dat file with error from the bootstrap run
  boot_dat=SS_readdat(file = "data.ss_new", version = "3.24", section = 3)
  
  #read the old bootstrap data file (i.e. from previous time step). In the case of the first time step, this is the original .dat file
  if (tstep ==1){
    boot_old = boot_dat
    boot_new = boot_old
  } else {
    boot_file = paste(pdir, hs, hcr, scn, itr,"/",(tstep-1),"/EM/EMdat.ss", sep="")
    boot_old = SS_readdat(file = boot_file, version = "3.24")
    #Add the new three random years of catches to the old bootstrap file (note that there are four seasons, so the total number of years to read is 12)
    boot_new = boot_old
    indi = dim(boot_dat$catch)[1]-11
    indf = dim(boot_dat$catch)[1]
    boot_new$catch = rbind(boot_old$catch, boot_dat$catch[indi:indf,])
    
    #There are three survey indeces, for survey 40 and 41
    #Extra the old CPUE data and the new bootstrap
    cpue40_old = boot_old$CPUE %>% filter(index==40)
    cpue41_old = boot_old$CPUE %>% filter(index==41)
    cpue40_dat = boot_dat$CPUE %>% filter(index==40)
    cpue41_dat = boot_dat$CPUE %>% filter(index==41)
    
    #extract the latest three years for the cpue survey
    indi40 = dim(cpue40_dat)[1]-2
    indf40 = dim(cpue40_dat)[1]
    indi41 = dim(cpue41_dat)[1]-2
    indf41 = dim(cpue41_dat)[1]
    cpue40_new = cpue40_dat[indi40:indf40,]
    cpue41_new = cpue41_dat[indi41:indf41,]
    
    i4041 = which(boot_old$CPUE$index==40|boot_old$CPUE$index==41)
    #Add the new length composition data. This is for 13 fleets, some of which have multiple seasons
    len_new = boot_dat$lencomp %>% filter(Yr== (boot_dat$endyr-2) |Yr== (boot_dat$endyr-1) |Yr== boot_dat$endyr)
    
    if (cpue40_old$year[length(cpue40_old$year)]==cpue40_new$year[length(cpue40_new$year)]){
      boot_new$CPUE = rbind(boot_old$CPUE[-i4041,], cpue40_old, cpue41_old)
      len_tot = boot_old$lencomp
    } else {
      boot_new$CPUE = rbind(boot_old$CPUE[-i4041,], cpue40_old, cpue40_new, cpue41_old, cpue41_new)
      len_tot = rbind(boot_old$lencomp, len_new)
    }
    
    boot_new$lencomp = len_tot
    
    #modify start year
    boot_new$endyr = boot_dat$endyr
    
    #change the length of the catch/CPUE/lencomp record
    #Note that there are catch records for every season in a year, for NPALB seasons=4
    boot_new$N_catch = boot_dat$N_catch
    boot_new$N_cpue = dim(boot_new$CPUE)[1]
    boot_new$N_lencomp = dim(boot_new$lencomp)[1]
  }
  
  path_dat = paste(pdir, hs, hcr, scn, itr,"/",tstep,"/EM/EMdat.ss",sep="")
  SS_writedat(boot_new, path_dat,version = "3.24")
  
  #***************************COPY FORECAST FILE*************************************
  #Move to the hcr directory 
  setwd(paste(pdir, hs, hcr, sep=""))
  
  command_mv = paste("for %I in (forecast.ss) do copy %I ", pwin, hsw, hcrw, scnw, itr, "\\",tstep,"\\EM\\", sep ="")
  shell(cmd = command_mv)
  
#***************************CHANGE CTL FILE*************************************
  ctl_in = paste(sdir, scn, "SAM/NPALB_ctl_20170416.txt", sep = "")
  ctl_out = paste(pdir, hs, hcr, scn, itr, "/",tstep,"/EM/EM.ctl", sep="")
  
  blk_end = 2015 + asmt_t[tstep] +2
  
  #change control file
  change_ctl(ctl_in, ctl_out, blk_end, rvar = 1, vadj = 1, nfleet = 41)
  
#*************************CHANGE STARTER FILE****************************************
  #read into R the ss starter file using the r4ss SS_readstarter function
  starter_dat=SS_readstarter(paste(pdir, hs, "starter.ss",sep=""))
  
  #specify the new dat file name
  starter_dat$datfile = "EMdat.ss"
  
  #specify the new ctl file name
  starter_dat$ctlfile = "EM.ctl"
  
  #specify to not use the ss3.par as initial parameter values
  starter_dat$init_values_src = 0
  
  #turn off estimation of parameters 
  #starter_dat$last_estimation_phase = 0
  
  #write new starter file
  dir_start = paste(pdir, hs, hcr, scn, itr, "/", tstep,"/EM/", sep = "")
  SS_writestarter(starter_dat, dir_start)
  
#*************************RUN THE EM MODEL*************************************
  
  #generate the .bat file to run the model
  Path = paste(pdir, hs, hcr, scn, itr, "/", tstep,"/EM/", sep = "")
  filename_em  <-paste(Path,"runem.bat",sep="")
  batchtext_em = paste(pwin,"SS_model\\ss3.exe -nohess -cbs 5000000000", sep="")
  writeLines(batchtext_em,filename_em)
  
  setwd(Path)
  command_run_em="runem.bat"
  shell(cmd= command_run_em)
  
  
}
  
  