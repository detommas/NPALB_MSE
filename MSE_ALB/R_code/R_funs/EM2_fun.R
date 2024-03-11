EM2_fun <- function(pdir, jdir, jwin, hs, hcr, scn, hsw, hcrw, scnw, pwin, itrv,it){
  itr=itrv[it]
  for (tstep in 1:10){
    
    #create directory for the new estimation model
    setwd(paste(pdir, hs, hcr, scn, itr,"/", tstep, sep = ""))
    cmddir = "mkdir EM2"
    shell(cmd = cmddir)
    
    setwd(paste(pdir, hs, hcr, scn, itr,"/", tstep, "/EM",sep = ""))
    
    #move files to run estimation model
    command_mv = paste("for %I in (forecast.ss,EMdat.ss,starter.ss) do copy %I ", pwin, hsw, hcrw, scnw, itr, "\\",tstep,"\\EM2\\", sep ="")
    shell(cmd = command_mv)
    
    #move the SS3.exe file so the ss3.cor file is found on the EM2 folder
    setwd(paste(jdir, "SS_model",sep = ""))
    
    #move files to run estimation model
    command_mv2 = paste("for %I in (SS3.exe.exe) do copy %I ", pwin, hsw, hcrw, scnw, itr, "\\",tstep,"\\EM2\\", sep ="")
    shell(cmd = command_mv2)
    
    #change the control file to get the full cor output
    ctl_in = paste(pdir, hs, hcr, scn, itr, "/",tstep,"/EM/EM.ctl", sep="")
    ctl_out = paste(pdir, hs, hcr, scn, itr, "/",tstep,"/EM2/EM.ctl", sep="")
    
    pattern = "#  0 # F_ballpark_lambda"
    ctl = readLines(ctl_in, warn = FALSE)
    which.line = grep(pattern=pattern, x=ctl)+1
    blk1.old = ctl[which.line]
    blk1.new = gsub("0 ", "1 ", blk1.old) # replace last year of block 1 
    ctl[which.line] = blk1.new
    which.line2 = grep(pattern=pattern, x=ctl)+2
    blk1.old2 = ctl[which.line2]
    blk1.new2 = gsub(" # 0 1 -1 5 1 5 1 -1 5 #", "0 0 0 0 0 0 -1 -1 15 #", blk1.old2) # replace last year of block 1 
    ctl[which.line2] = blk1.new2
    which.line3 = grep(pattern=pattern, x=ctl)+6
    blk1.old3 = ctl[which.line3]
    blk1.new3 = gsub("999", "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15", blk1.old3) # replace last year of block 1 
    ctl[which.line3] = blk1.new3
    which.line4 = grep(pattern=pattern, x=ctl)+7
    ctl[which.line4] = 999
    
    ## Write new ctl file to directory
    writeLines(ctl, con = ctl_out)
    
    #generate the .bat file to run the model
    Path = paste(pdir, hs, hcr, scn, itr, "/", tstep,"/EM2/", sep = "")
    #filename_em  <-paste(Path,"runem.bat",sep="")
    batchtext_em = paste("ss3.exe -cbs 5000000000", sep="")
    writeLines(batchtext_em,filename_em)
    
    setwd(Path)
    #command_run_em="runem.bat"
    shell(cmd= command_run_em)
    
    #read the .cor files
    tmp6=read.admbFit("ss3")
    id=which(tmp6$names=="Extra_Std")
    cormat1=data.frame(index=id,name=tmp6$names[id],value=tmp6$est[id],std.dev=tmp6$std[id])
    cormat=cbind(cormat1, tmp6$cor[id,id])
    
    #save the correlation info for input into projection software
    write.table(cormat,"corr_naa.txt",row.names=FALSE)
  }
}