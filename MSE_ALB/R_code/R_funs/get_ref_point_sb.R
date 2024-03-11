#Get reference point SSB**************************************************************************************************************************************
#Developed by H. Ijima for ISC NPALB Projection Software

get_ref_point_sb = function(myreplist,startyr,endyr,spr) {
  
  tmp1 = myreplist$Dynamic_Bzero %>% filter(Yr>=startyr, Yr<=endyr) %>%
    summarise(sbf0=mean(SSB_nofishing))
  
  tmp2 = myreplist$equil_yield[,3:11] %>% mutate(diff=(SPR-spr)^2) %>%
    filter(diff==min(diff))
  
  refpoint_b = data.frame(sbf0 = tmp1[1,1], fspr_b = tmp2[1,6])
  
  return(refpoint_b)
  
  rm(tmp1,tmp2)
  
}