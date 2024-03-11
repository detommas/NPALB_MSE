#Function to extract numbers at age 
#created by H. Ijima for the NPALB Projection software
get_naa = function(myreplist) {
  
  natage = myreplist$natage
  startyr = myreplist$startyr
  endyr = myreplist$endyr
  
  tmp1 = natage %>% filter(`Beg/Mid`=="B",Yr>=startyr) %>% select(-Area,-Bio_Pattern,-BirthSeas,-Platoon,-Morph,-Time,-Era,-`Beg/Mid`)
  tmp2 = reshape2::melt(tmp1, c("Yr","Sex","Seas"))
  tmp3 = tmp2 %>% rename(age=variable,pop_n=value) %>% filter(Seas==2,Yr==endyr) %>% arrange(Sex,Seas) %>% select(pop_n)
  naa = tmp2 %>% rename(age=variable,pop_n=value) %>% filter(Yr<=endyr) %>% arrange(Sex,Seas)
  int_n = tmp3$pop_n
  
  rm(tmp1,tmp2,tmp3)
  
  list(naa=naa,int_n=int_n)
  
}
