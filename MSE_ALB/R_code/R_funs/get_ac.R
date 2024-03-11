#Function to calcualte autocorrelation of recruitment estimates from SS output
#created by H. Ijima for the NPALB Projection software
get_ac = function(myreplist) {
  
  startyr = myreplist$startyr
  
  tmp1 = myreplist$recruitpars %>% filter(Yr>=startyr)
  tmp2 = acf(tmp1$Value)
  ac = tmp2$acf[2]
  return(ac)
  
}
