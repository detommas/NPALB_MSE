#' Add to a Stock Synthesis data file new catch data
#'
#' This function changes an SS dat file to
#' 1) add more catch data
#' 2) changes the numbers of catch data
#' 3) changes the end year
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

change_dat_boot_scn9 <- function(ss_file_in, ss_file_out, t_asmt, nrep, cdat_new,tstep){
  
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
  
  #Write a stock synthesis data file 
  SS_writedat(om_dat_new, ss_file_out, version = "3.24")
  
}
