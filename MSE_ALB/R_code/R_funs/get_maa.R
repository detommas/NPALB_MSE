#Function to extract mortality, maturity, and weight at age afrom SS output
#created by H. Ijima for the NPALB Projection software
get_maa = function(myreplist) {
  
  endgrowt = myreplist$endgrowth #Wt_Beg=waa in season 2
  endgrowt = endgrowt %>% filter(Seas==2)
  bio = endgrowt %>% select(Wt_Beg,Age_Mat) %>% mutate(Age_Mat=ifelse(Age_Mat<=0, 0, Age_Mat))
  waa = bio$Wt_Beg
  mat = bio$Age_Mat
  age = myreplist$accuage
  gender = myreplist$nsexes
  #maa = rep(0.3,(age+1)*gender)
  maa = c(1.36, 0.56, 0.45, 0.48, 0.48, 0.48, 0.48, 0.48, 0.48, 0.48, 0.48, 0.48, 0.48, 0.48, 0.48, 0.48,
          1.36, 0.56, 0.45, 0.39, 0.39, 0.39, 0.39, 0.39, 0.39, 0.39, 0.39, 0.39, 0.39, 0.39, 0.39, 0.39)
  
  list(waa=waa,mat=mat,age=age,gender=gender,maa=maa)
  
}
