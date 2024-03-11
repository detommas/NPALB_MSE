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

EM_fun_newftgt <- function(pdir, sdir, hs, hcr, scn, hsw, hcrw, scnw, pwin, itr, tstep,hcrnum){
 
  #create directory for the second estimation model
  setwd(paste(pdir, hs, hcr, scn, itr,"/", tstep, sep = ""))
  cmddir = "mkdir EM2"
  shell(cmd = cmddir)
  
  #***************************COPY DAT FILE*************************************
  #move to directory with EM output
  setwd(paste(pdir, hs, hcr, scn, itr,"/", tstep, "/EM/", sep = ""))
  
  command_mv = paste("for %I in (EMdat.ss) do copy %I ", pwin, hsw, hcrw, scnw, itr, "\\",tstep,"\\EM2\\", sep ="")
  shell(cmd = command_mv)
  
  #***************************COPY CTL FILE*************************************
  #move to directory with EM output
  setwd(paste(pdir, hs, hcr, scn, itr,"/", tstep, "/EM/", sep = ""))
  
  command_mv = paste("for %I in (control.ss_new) do copy %I ", pwin, hsw, hcrw, scnw, itr, "\\",tstep,"\\EM2\\", sep ="")
  shell(cmd = command_mv)
  
  #***************************CHANGE FORECAST FILE*************************************
  #Move to the hcr directory 
  fdir = (paste(pdir, hs, hcr,"forecast.ss", sep=""))
  fout= (paste(pdir, hs, hcr, scn, itr,"/", tstep, "/EM2/forecast.ss", sep = ""))
  
  pattern = "#_SPRtarget"
  fst = readLines(fdir, warn = FALSE)
  sprv = read.csv(paste(sdir,scnnum,"/sprv.csv", sep = ""))
  new_spr = round(sample(sprv[,1],1),digits = 2)
  
  #Change block 1
  which.line = grep(pattern=pattern, x=fst)
  spr.old = fst[which.line]
  if (hcrnum %in% c(6,7,8,14,15,16)){gspr="0.4"} else{gspr="0.5"}
  spr.new = gsub(gspr, new_spr, spr.old) # replace spr with random historical
  fst[which.line] = spr.new
  
  ## Write it back to file
  writeLines(fst, con = fout)
#*************************CHANGE STARTER FILE****************************************
  #read into R the ss starter file using the r4ss SS_readstarter function
  starter_dat=SS_readstarter(paste(pdir, hs, "starter.ss",sep=""))
  
  #specify the new dat file name
  starter_dat$datfile = "EMdat.ss"
  
  #specify the new ctl file name
  starter_dat$ctlfile = "control.ss_new"
  
  #specify to not use the ss3.par as initial parameter values
  starter_dat$init_values_src = 0
  
  #turn off estimation of parameters 
  starter_dat$last_estimation_phase = 0
  
  #write new starter file
  dir_start = paste(pdir, hs, hcr, scn, itr, "/", tstep,"/EM2/", sep = "")
  SS_writestarter(starter_dat, dir_start)
  
#*************************RUN THE EM MODEL*************************************
  
  #generate the .bat file to run the model
  Path = paste(pdir, hs, hcr, scn, itr, "/", tstep,"/EM2/", sep = "")
  filename_em  <-paste(Path,"runem.bat",sep="")
  batchtext_em = paste(pwin,"SS_model\\ss3.exe -nohess -cbs 5000000000", sep="")
  writeLines(batchtext_em,filename_em)
  
  setwd(Path)
  command_run_em="runem.bat"
  shell(cmd= command_run_em)
  
  
}
  
  