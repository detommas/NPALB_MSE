#' Prepares the needed files to run the NPALB OM model in SS in bootstrap mode and runs the model
#' This produces data files with error for input into EM  
#' 
#' @param pdir the parent directory path
#' @param sdir path to directory of OM (initial)
#' @param hs the harvest strategy being run
#' @param hcr the hcr being run
#' @param scn the scenario being run
#' @param hsw the harves tstrategy being run in Windows notation
#' @param hcrw the hcr being run in Winodws notation
#' @param scnw the scenario being run in Windows notation
#' @param pwin he parent directory path in Windows notation
#' @param itr iteration number
#' @param tstep time step of the OM
#' @param tasmt frequency of assessments
#' @param new_cdat new catch data obtained from TAC and catch ratios
#' @param rec_devsn new recruitment deviations
#' @param sel_devs new age selectivity deviations

#' @author Desiree Tommasi

OMBoot_fun_tvry_scn9 <- function(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep, tasmt, new_cdat, rec_devs, sel_devs){
 
  #create directory for the Bootstrap routine
  setwd(paste(pdir, hs, hcr, scn, itr,"/",tstep, sep = ""))
  cmddir = "mkdir Boot"
  shell(cmd = cmddir)
  
#*****************************COPY FORECAST FILE FROM HCR FOLDER*******************************************   
  #Move to the hcr directory 
  setwd(paste(pdir, hs, hcr, sep=""))
  
  command_mv = paste("for %I in (forecast.ss) do copy %I ", pwin, hsw, hcrw, scnw, itr, "\\",tstep,"\\Boot\\", sep ="")
  shell(cmd = command_mv)
  
#*****************************CHANGE PAR FILE**********************************************
  #include recruitment and selectivity deviations for future time steps
  
  #Specify inputs to change_rec_devs_alb function
  if (tstep == 1){
    pfile_in = paste(sdir,scn, "ss3.PAR",sep="")
  } else {
    pfile_in = paste(pdir, hs, hcr, scn, itr,"/",(tstep-1),"/Boot/ss3.PAR", sep = "")
  }
  
  pfile_out = paste(pdir, hs, hcr, scn, itr, "/",tstep,"/Boot/ss3.PAR", sep="")
  
  #Modify par file to include the recruitment deviations for the next assessement cycle
  rec_devsn = rec_devs[asmt_t[tstep]:((asmt_t[tstep])+2)]
  change_rec_devs_alb(recdevs_new = rec_devsn, par_file_in = pfile_in, par_file_out = pfile_out)
  
  #Specify inputs to change_selex_devs_alb function
  sfile_in = pfile_out #the input file would be the par file with the updated rec devs
  
  sfile_out = paste(pdir, hs, hcr, scn, itr, "/",tstep,"/Boot/ss3.PAR", sep="")
  
  #Modify par file to include the time varying selectivity deviations for the next assessement cycle
  sel_devsn = sel_devs[asmt_t[tstep]:((asmt_t[tstep])+2),]
  change_selex_devs_alb(seldevs_new = sel_devsn, par_file_in = sfile_in, par_file_out = sfile_out, na=4)
  
#*****************************CHANGE DAT FILE*******************************************   
  # Enter new catch data given the TAC and put dummy values for CPUE and size compositions
  
  #Specify inputs to the change_dat function
  if (tstep == 1){
    file_in = paste(sdir,scn,"NPALB_dat_20180131.txt",sep="")
  } else {
    file_in = paste(pdir, hs, hcr, scn, itr,"/",(tstep-1),"/Boot/Bootdat.txt",sep="")# file in for catch, as it should be the true one up to the time of the assessment
  }
  
  file_out= paste(pdir, hs, hcr, scn, itr,"/", tstep,"/Boot/Bootdat.txt",sep="")
  
  #if the TAC is 0, then no CPUE or size comps possible as no fish were caught, use the change catch only code, if not also add dummy values for CPUE and size comps
  if (sum(new_cdat[,2:30])==0){
    #Insert new catch data into .dat file
    change_dat_boot_scn9(ss_file_in = file_in, ss_file_out = file_out, t_asmt = tasmt, nrep = tasmt, cdat_new = new_cdat,tstep=tstep)
  } else {
    change_dat_cpue_boot_scn9(ss_file_in = file_in, ss_file_out = file_out, t_asmt = tasmt, nrep = tasmt, cdat_new = new_cdat,tstep=tstep)
  }
  
  #***************************CHANGE CTL FILE*************************************
  #Modify end of blocks, end of main recruitment deviations, and set rec devs as fixed in control file
  blk_in = paste(sdir, scn, "NPALB_ctl_20170416.txt", sep = "")
  blk_out = paste(pdir, hs, hcr, scn, itr, "/",tstep,"/Boot/Boot.ctl", sep="")
  
  blk_end = 2015 + asmt_t[tstep] +2
  
  #change control file
  change_ctl(blk_in, blk_out, blk_end, rvar = 1, vadj = 2, nfleet = 41)
  
#*************************CHANGE STARTER FILE****************************************
  #Modify starter file to run OM with new catch data without estimating parameters 
  
  #read into R the ss starter file using the r4ss SS_readstarter function
  starter_dat=SS_readstarter(paste(pdir, hs, "starter.ss", sep=""))
  
  #specify the new dat file name
  starter_dat$datfile = "Bootdat.txt"
  
  #specify the new ctl file name
  starter_dat$ctlfile = "Boot.ctl"
  
  #specify to use the ss3.par as parameters
  starter_dat$init_values_src = 1
  
  #turn off estimation of parameters 
  starter_dat$last_estimation_phase = 0
  
  #add 1 data bootstrap file
  starter_dat$N_bootstraps = 3
  
  #write new starter file
  path_start = paste(pdir, hs, hcr, scn, itr,"/",tstep,"/Boot/",sep="")
  SS_writestarter(starter_dat, path_start)
  
#*************************RUN THE BOOTSTRAP MODEL*************************************
  
  #generate the .bat file to run the model
  Path = paste(pdir, hs, hcr, scn, itr, "/", tstep,"/Boot/", sep="")
  filename_om  <-paste(Path,"ssnohess.bat",sep="")
  batchtext_om = paste(pwin,"SS_model\\ss3.exe -nohess -cbs 5000000000",sep="")
  writeLines(batchtext_om,filename_om)
  
  setwd(Path)
  command_run_om="ssnohess.bat"
  shell(cmd= command_run_om)
  
  
}
  
  