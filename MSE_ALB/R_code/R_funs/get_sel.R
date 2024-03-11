#Function to get fishing mortality at age to input into ISC NPALB Projection software v2020
#Developed by H. Ijima 

get_sel = function(myreplist,startyr,endyr) {
  
  tmp1 = data.frame(Fleet=myreplist$fleet_ID, fleet_type=myreplist$IsFishFleet, fleet=myreplist$FleetNames) #%>% filter(fleet_type==1)
  tmp2 = myreplist$ageselex %>% filter(Factor=="Asel2", Yr>=startyr, Yr<=endyr)
  tmp3 = left_join(tmp1, tmp2, by = "Fleet") %>% filter(fleet_type==TRUE) %>%
    select(-fleet_type, -Factor, -Morph, -Label) %>%
    pivot_longer(names_to="age", values_to="sel", col=c(-Fleet, -fleet, -Yr, -Seas, -Sex)) %>%
    mutate(fleet=as.character(fleet))
  
  tmp4 = myreplist$exploitation %>% filter(Yr>=startyr, Yr<=endyr) %>%
    select(-F_std) %>%
    pivot_longer(names_to="fleet", values_to="f", col=c(-Yr, -Seas)) 
  
  tmp5 = tmp3 %>% select(-Fleet) %>%
    mutate(age=as.numeric(age)) %>%
    group_by(fleet,Seas,Sex,age) %>%
    summarise(sel=mean(sel))
  
  tmp6 = tmp4 %>% group_by(Seas, fleet) %>%
    summarise(f=mean(f))
  
  sel = left_join(tmp5,tmp6, by=c("fleet", "Seas")) %>%
    mutate(faa=f*sel) %>%
    group_by(Seas,Sex,age) %>%
    summarise(faa=sum(faa)) %>%
    ungroup() %>%
    mutate(sel=faa/sum(tmp6$f)*0.25)
  
  #tmp5 = tmp4 %>% group_by(Yr) %>%
  #               summarise(tot=sum(f))
  
  #tmp6 = left_join(tmp4,tmp5) %>% mutate(f=f/tot*0.25)
  
  
  #sel = left_join(tmp3, tmp6, by = c("fleet", "Yr", "Seas")) %>%
  #      mutate(age=as.numeric(age), faa=sel*f) %>%
  #     group_by(fleet, Seas, age, Sex) %>%
  #     summarise(faa=mean(faa)) %>%
  #     ungroup() %>%
  #    group_by(Seas, age, Sex) %>%
  #     summarise(faa=sum(faa)) %>%
  #     ungroup() %>%
  #    arrange(Seas, Sex)
  
  return(sel)
  
  rm(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6)
  
}