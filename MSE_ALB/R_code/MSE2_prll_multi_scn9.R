#North Pacific Albacore MSE Wrapper code 

#this runs the NPALB MSE framework in parallel for the specified number of iterations
#iterations differ in their recruitment, time-selectivity deviations, and random implementation errors

#clean up the workspace
rm(list=ls())

#source all the packages needed
library(foreach)
library(doParallel)
library(r4ss)
library(Rcpp)
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)
library(data.table)
library(gridExtra)

#Specify path of parent directory
pdir = "C:/Users/desiree.tommasi/Documents/Albacore_MSE/Server/MSE_ALB/"
#set working directory to where all functions needed are stored
setwd(paste(pdir,"R_code/R_funs", sep = ""))
#Specify the path of conditioned initial OM
sdir = "C:/Users/desiree.tommasi/Documents/Albacore_MSE/Server/MSE_ALB/"

#source all the functions
file.sources = list.files()
sapply(file.sources,source,.GlobalEnv)

#source NPALB future projection code
setwd(pdir)
worker.init = function (){
  library(Rcpp)
  Sys.setenv("PKG_CXXFLAGS"="-std=c++0x") 
  sourceCpp("ssf2_V2.cpp") #Read future projection program
}

#Sys.setenv("PKG_CXXFLAGS"="-std=c++0x") 
#sourceCpp("ssf2_V2.cpp") #Read future projection program

#we are projecting 30 years into the future
#Since the albacore assessment occurs every 3 years, there are 10 assessement times 
#below we specify when those haapen in the 30 years time series
asmt_t = seq(1, 30, by=3)

#specify the frequency of assessements
tasmt = 3

#import needed files
#catch ratios by fleet and season obtained by running the catch_ratios.R code 
cr_hcr = read.table(paste(pdir,"cr_hcr.txt", sep = ""))
#catch ratios by country/gear rather than fleet 
cr_c = read.table(paste(pdir,"cr_c.txt", sep = ""))
#biomass to numbers conversion factor by year and season for fleets 5-8, 11-12, and 14-15 (obtained with catch_ratios.R)
ba_conv = read.table(paste(pdir,"ba_conv.txt", sep = ""))

#Set the harvest strategy
hsnum = 4
#Set the scenario
scnnum=1

#Calculate the numbers of cores 
no_cores = detectCores() - 1

#Initiate cluster
cl = makeCluster(no_cores)
clusterCall(cl, worker.init)
registerDoParallel(cl)


#Set the HCR
hn=1
bt = c(0.3,0.3,0.3,0.2,0.2,0.2,0.2,0.14,0.3,0.3,0.3,0.2,0.2,0.2,0.2,0.14)
bl = c(0.2,0.14,0.077,0.14,0.077,0.14,0.077,0.077,0.2,0.14,0.077,0.14,0.077,0.14,0.077,0.077)
tm = c(0.25,0.25,0,0.25,0,0.25,0,0,0.5,0.5,0.25,0.5,0.25,0.5,0.25,0.25)
pr = c(0.8,rep(0.9,7),0.8,rep(0.9,7))

  hcrnum=hn
  Bthr = bt[hn] 
  Blim = bl[hn]
  Tmin = tm[hn]
  pl = pr[hn]
  
  #Run the MSE code for the specified number of iterations
  #rdev1 is the last recruitment deviation from year 2015 in the 2017 assessment model
  #it is needed to compute the first random deviation with autocorrelation
  #Bthr is the SSB based biomass reference point. It represents the fraction of unfished dynamic SSB
  #Blim is the limit reference point represting the fraction of unfished dynamic SSB
  #Note that the F based TRP is specified already in the forecast file 
  #the output is alredy saved as the code runs in the respective folders
  #main output to then compute performance metrics is the outlist.text file created for each iteration
  foreach(itr = 1:1, .packages = c('r4ss','Rcpp','dplyr','reshape2'),.noexport = "ssf2_V2.cpp") %dopar% { NPALB_MSE2_hs3_tvry_scn9(hsnum,hcrnum,scnnum,itr, rdev1 = -4.35459e-06, Bthr = Bthr, Blim = Blim,Tmin=Tmin, pl = pl) }

stopCluster(cl)
