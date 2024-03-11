#Function to extract stock recruitment function parameters from SS output
#created by H. Ijima for the NPALB Projection software
get_rec = function(myreplist) {
  
  parameters = myreplist$parameters
  tmp1 = parameters %>% filter(Label=="SR_LN(R0)") %>% select(Value) %>% mutate(Value=exp(Value))
  tmp2 = parameters %>% filter(Label=="SR_BH_steep") %>% select(Value)
  tmp3 = parameters %>% filter(Label=="SR_sigmaR") %>% select(Value)
  
  Rzero = tmp1$Value
  h = tmp2$Value
  sigmar = tmp3$Value
  tmp4 = myreplist$Dynamic_Bzero %>% filter(Era=="VIRG") %>% select(SSB) 
  SBzero = tmp4$SSB
  
  rm(tmp1,tmp2,tmp3,tmp4)
  
  list(Rzero=Rzero,h=h,sigmar=sigmar,SBzero=SBzero)
  
}