#' Changes the NPALB Stock Synthesis ctl file to
#' 1) reflect the terminal year of the current tstep
#' 2) change the last year of the recruitment deviations
#' 3) set recruitment deviations to be fixed parameters or simple deviations
#' 4) change variance adjustment
#'
#' @param ss_file_in filename of original ctl file to be modified, with full path or relative to working directory
#' @param ss_file_out filename for the new ctl file with full path or relative to working directory
#' @param new_end new end year
#' @param rvar specify how rec vars are coded if set to (1) as simple deviations, if rvar is to 2, it is -1 and recruitment deviations are fixed (2)
#' @param vadj specify if this should left as in assessment model (1) or set to 0 and 1 (2) - option 2 is used to create bootstrap files
#' @param nfleet specifiy the number of fleets in the model (including CPUE)
#' @return A modified ctl file.
#' @author Desiree Tommasi

change_ctl = function(ss_file_in, ss_file_out, new_end, rvar, vadj, nfleet){

  pattern = "# begin and end years of blocks"
  pattern2 = "# first year of main recr_devs"
  pattern3 = "# last year of main recr_devs"
  pattern4 = "#_add_to_survey_CV"
  pattern5 = "#_mult_by_lencomp_N"
  pattern6 = "#_first_yr_fullbias_adj_in_MPD"
  pattern7 = "#_last_yr_fullbias_adj_in_MPD"
  
  ctl = readLines(ss_file_in, warn = FALSE)
  new_end_c = as.character(new_end)
  new_end_b = as.character(new_end+1)
  
  #Change block 1
  which.line = grep(pattern=pattern, x=ctl)+1
  blk1.old = ctl[which.line]
  blk1.new = gsub("2015", new_end_c, blk1.old) # replace last year of block 1 
  ctl[which.line] = blk1.new
  #Change block 2
  which.line = grep(pattern=pattern, x=ctl)+2
  blk1.old = ctl[which.line]
  blk1.new = gsub("2015", new_end_c, blk1.old) # replace last year of block 2 
  ctl[which.line] = blk1.new
  #Change block 3
  which.line = grep(pattern=pattern, x=ctl)+3
  blk1.old = ctl[which.line]
  blk1.new = gsub("2015", new_end_c, blk1.old) # replace last year of block 3 
  ctl[which.line] = blk1.new
  #Change block 4
  which.line = grep(pattern=pattern, x=ctl)+4
  blk1.old = ctl[which.line]
  blk1.new = gsub("2015", new_end_c, blk1.old) # replace last year of block 4 
  ctl[which.line] = blk1.new
  #Change block 5
  which.line = grep(pattern=pattern, x=ctl)+5
  blk1.old = ctl[which.line]
  blk1.new = gsub("2015", new_end_c, blk1.old) # replace last year of block 5 
  ctl[which.line] = blk1.new
  
  #change end year of recruitment deviations
  which.line = grep(pattern=pattern2, x=ctl)+1
  end.old = ctl[which.line]
  end.new = gsub("2015", new_end_c, end.old) # replace last year of block 1
  ctl[which.line] = end.new
  
  #set so recruitment deviations are not estimated (not used if rvar=1)
    which.line = grep(pattern=pattern3, x=ctl)+1
    rec.old = ctl[which.line]
    rec.new = gsub("1", "-1", rec.old) # set to re devs as fixed
    
    if (rvar == 2) {ctl[which.line] = rec.new}
    
    #change the last year of bias adjustment
    which.line = grep(pattern=pattern6, x=ctl)+1
    end.old = ctl[which.line]
    end.new = gsub("2011.5", new_end_c, end.old) # replace last year of block 1
    ctl[which.line] = end.new
    
    #change the last year of no bias adjustment
    which.line = grep(pattern=pattern7, x=ctl)+1
    end.old = ctl[which.line]
    end.new = gsub("2014.9", new_end_b, end.old) # replace last year of block 1
    ctl[which.line] = end.new
  
  #Change variance adjustment to additive variables to 0 and for multiplicative to 1
  #before running the bootstrap
  which.line = grep(pattern=pattern4, x=ctl)
  sur.old = ctl[which.line]
  sur.old <- gsub("^\\s+|\\s+$", "", sur.old) # remove leading blank
  sur.old <- gsub("\\s+", " ", sur.old)       # remove >1 blanks
  sur.old <- as.numeric(unlist(strsplit(sur.old, split= " ")))
  #as NA introduced where non digits only select 
  sur.old = sur.old[1:nfleet]
  rep0 = rep(0,nfleet)
  sur.new = paste0(" ", rep0 ,collapse="")
  sur.new = paste(sur.new, "#_add_to_survey_CV")
  if (vadj == 2) {ctl[which.line] = sur.new}
  
  which.line = grep(pattern=pattern5, x=ctl)
  comp.old = ctl[which.line]
  comp.old <- gsub("^\\s+|\\s+$", "", comp.old) # remove leading blank
  comp.old <- gsub("\\s+", " ", comp.old)       # remove >1 blanks
  comp.old <- as.numeric(unlist(strsplit(comp.old, split= " ")))
  #as NA introduced where non digits only select 
  comp.old = comp.old[1:nfleet]
  rep1 = rep(1,nfleet)
  comp.new = paste0(" ", rep1 ,collapse="")
  comp.new = paste(comp.new, "#_mult_by_lencomp_N")
  if (vadj == 2) {ctl[which.line] = comp.new}
  
  #change end of age selectivity deviations
  pattern8 = "# AgeSel_27P_2_F27_EPOSF"
  which.line = grep(pattern=pattern8, x=ctl)
  blk1.old = ctl[which.line]
  blk1.new = gsub("2015", new_end_c, blk1.old) # replace end of time varying age selectivities 
  ctl[which.line] = blk1.new
  
  pattern9 = "# AgeSel_27P_3_F27_EPOSF"
  which.line = grep(pattern=pattern9, x=ctl)
  blk1.old = ctl[which.line]
  blk1.new = gsub("2015", new_end_c, blk1.old) # replace end of time varying age selectivities 
  ctl[which.line] = blk1.new
  
  pattern10 = "# AgeSel_27P_4_F27_EPOSF"
  which.line = grep(pattern=pattern10, x=ctl)
  blk1.old = ctl[which.line]
  blk1.new = gsub("2015", new_end_c, blk1.old) # replace end of time varying age selectivities 
  ctl[which.line] = blk1.new
  
  pattern11 = "# AgeSel_27P_5_F27_EPOSF"
  which.line = grep(pattern=pattern11, x=ctl)
  blk1.old = ctl[which.line]
  blk1.new = gsub("2015", new_end_c, blk1.old) # replace end of time varying age selectivities 
  ctl[which.line] = blk1.new
  
  
  ## Write it back to file
  writeLines(ctl, con = ss_file_out)
  
  }