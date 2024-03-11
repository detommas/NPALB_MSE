#Get reference point fmulti***********************************************************************************************************************************
#Developed by H. Ijima for ISC NPALB Projection software

get_ref_point_f = function(myreplist,startyr,endyr,spr) {
  
  tmp1 = myreplist$equil_yield[,4:12] %>% filter(Catch==max(Catch))
  tmp2 = myreplist$equil_yield[,4:12] %>% mutate(diff=(SPR-spr)^2) %>%
    filter(diff==min(diff))
  tmp3 = myreplist$exploitation %>% filter(Yr>=startyr, Yr<=endyr) %>%
    select(-F_std) %>%
    pivot_longer(names_to="fleet", values_to="f", col=c(-Yr, -Seas)) %>%
    group_by(Seas,fleet) %>%
    summarise(f=mean(f))
  
  refpoint = data.frame(fmsy = tmp1[1,1], msy = tmp1[1,9], fspr = tmp2[1,1], bench = sum(tmp3$f))
  
  rm(tmp1,tmp2,tmp3)
  
  return(refpoint=refpoint)
  
}