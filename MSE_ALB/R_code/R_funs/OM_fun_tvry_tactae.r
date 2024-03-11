#' Prepares the needed files to run the NPALB OM model in SS 
#' 
#' @param pdir the parent directory path 
#' @param sdir path to directory of OM (initial)
#' @param hs the harvest strategy being run
#' @param hcr the hcr being run
#' @param scn the scenario being run
#' @param hsw the harvest strategy being run in Windows notatation
#' @param hcrw the hcr being run in Windows notation
#' @param scnw the scenario being run in Windows notation
#' @param pwin he parent directory path in Windows notation
#' @param itr iteration number
#' @param tstep time step of the OM
#' @param tasmt frequency of assessments
#' @param new_cdat new catch data obtained from TAC
#' @param rec_devsn new recruitment deviations
#' @param sel_devsn new selectivity deviations
#' 
#' @author Desiree Tommasi

OM_fun_tvry_tactae <- function(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep, tasmt, new_cdat, rec_devs, sel_devs){
 
#*****************************CREATE DAT FILE*******************************************   
  #The OM catch data corresponds to the bootstrap data with no error
  
  #create directory for the operating model
  setwd(paste(pdir, hs, hcr, scn, itr,"/", tstep, sep = ""))
  cmddir = "mkdir OM"
  shell(cmd = cmddir)
  
  #move to directory with Bootstrap run
  setwd(paste(pdir, hs, hcr, scn, itr,"/", tstep, "/Boot/", sep = ""))
  
  #extract the data with no error from the bootstrap run
  boot_dat=SS_readdat(file = "data.ss_new", section = 2, version="3.24")
  
  #read the old bootstrap data file (i.e. from previous time step). In the case of the first time step, this is the boot.dat file
  if (tstep ==1){
    boot_old = boot_dat
    boot_new = boot_old
  } else {
    boot_file = paste(pdir, hs, hcr, scn, itr,"/",(tstep-1),"/OM/OMdat.ss", sep="")
    boot_old = SS_readdat(file = boot_file,version="3.24")
    #Add the new three random years of catches to the old bootstrap file (note that there are four seasons, so the total number of years to read is 4)
    boot_new = boot_old
    indi = dim(boot_dat$catch)[1]-((tasmt*4)-1)
    indf = dim(boot_dat$catch)[1]
    boot_new$catch = rbind(boot_old$catch, boot_dat$catch[indi:indf,])
    
    #There are three survey indeces, for survey 40 and 41
    #Extra the old CPUE data and the new bootstrap
    cpue40_old = boot_old$CPUE %>% filter(index==40)
    cpue41_old = boot_old$CPUE %>% filter(index==41)
    cpue40_dat = boot_dat$CPUE %>% filter(index==40)
    cpue41_dat = boot_dat$CPUE %>% filter(index==41)
    
    #extract the new cpue data
    indi40 = dim(cpue40_dat)[1]-(tasmt-1)
    indf40 = dim(cpue40_dat)[1]
    indi41 = dim(cpue41_dat)[1]-(tasmt-1)
    indf41 = dim(cpue41_dat)[1]
    cpue40_new = cpue40_dat[indi40:indf40,]
    cpue41_new = cpue41_dat[indi41:indf41,]
    
    i4041 = which(boot_old$CPUE$index==40|boot_old$CPUE$index==41)
    #Add the new length composition data. This is for 13 fleets, some of which have multiple seasons
    #generate the years for which to extract data
    yearb=1:tasmt
    ct=1
    for (j in (tasmt-1):0){
      yearb[ct] = boot_dat$endyr-j
      ct=ct+1
    }
    len_new = boot_dat$lencomp %>% filter(Yr %in% yearb)
    
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
  
  path_dat = paste(pdir, hs, hcr, scn, itr,"/",tstep,"/OM/OMdat.ss",sep="")
  SS_writedat(boot_new, path_dat,version="3.24")
  
#*****************************CHANGE PAR FILE**********************************************
  #include recruitment deviations for future time steps
  if (tstep == 1){
    pfile_in = paste(sdir,scn, "ss3.par",sep="")
  } else {
    pfile_in = paste(pdir, hs, hcr, scn, itr,"/",(tstep-1),"/OM/ss3.par", sep = "")
  }
  
  pfile_out = paste(pdir, hs, hcr, scn, itr, "/",tstep,"/OM/ss3.par", sep="")
  
  #Modify par file to include the recruitment deviations for the next assessement cycle
  rec_devsn = rec_devs[tstep]
  change_rec_devs_alb(recdevs_new = rec_devsn, par_file_in = pfile_in, par_file_out = pfile_out)
  
  #Specify inputs to change_selex_devs_alb function
  sfile_in = pfile_out #the input file would be the par file with the updated rec devs
  
  sfile_out = paste(pdir, hs, hcr, scn, itr, "/",tstep,"/Boot/ss3.par", sep="")
  
  #Modify par file to include the time varying selectivity deviations for the next assessement cycle
  sel_devsn = sel_devs[tstep,]
  change_selex_devs_alb_tactae(seldevs_new = sel_devsn, par_file_in = sfile_in, par_file_out = sfile_out, na=4)
  
#***************************CHANGE CTL FILE*************************************
  #Modify end of blocks, end of main recruitment deviations, and set rec devs as fixed in control file
  blk_in = paste(sdir, scn, "NPALB_ctl_20170416.txt", sep = "")
  blk_out = paste(pdir, hs, hcr, scn, itr, "/",tstep,"/OM/OM.ctl", sep="")
  
  blk_end = 2015 + tstep 
  
  #change control file
  change_ctl(blk_in, blk_out, blk_end, rvar = 1, vadj = 2, nfleet = 41)

#***************************COPY FORECAST FILE*************************************
  #Move to the hcr directory 
  setwd(paste(pdir, hs, hcr, sep=""))
  
  command_mv = paste("cp forecast.ss ", pdir, hs, hcr, scn, itr, "/",tstep,"/OM/", sep ="")
  shell(cmd = command_mv)
  
#*************************CHANGE STARTER FILE****************************************
  #Modify starter file to run OM with new catch data without estimating parameters 
  
  #read into R the ss starter file using the r4ss SS_readstarter function
  starter_dat=SS_readstarter(paste(pdir, hs, "starter.ss", sep=""))
  
  #specify the new dat file name
  starter_dat$datfile = "OMdat.ss"
  
  #specify the new dat file name
  starter_dat$ctlfile = "OM.ctl"
  
  #specify to use the ss3.par as parameters
  starter_dat$init_values_src = 1
  
  #turn off estimation of parameters 
  starter_dat$last_estimation_phase = 0
  
  #write new starter file
  path_start = paste(pdir, hs, hcr, scn, itr,"/",tstep,"/OM/",sep="")
  SS_writestarter(starter_dat, path_start)
  
#*************************RUN THE OM MODEL*************************************
  
  #generate the .bat file to run the model
  #Path = paste(pdir, hs, hcr, scn, itr, "/", tstep,"/OM/", sep="")
  #filename_om  <-paste(Path,"ssnohess.bat",sep="")
  #batchtext_om = paste(pwin,"SS_model\\ss3.exe -nohess -cbs 500000000",sep="")
  #writeLines(batchtext_om,filename_om)
  
  #setwd(Path)
  #command_run_om="ssnohess.bat"
  #shell(cmd= command_run_om)
  
  
}
  
  