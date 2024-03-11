#M at age and Maturity at age*********************************************************************************************************************************
#Developed by H. Ijima for ISC NPALB projection software v2020
#Some modifications added by D. Tommasi to make it work with SS3.24

get_maa2 = function(myreplist) {
  
  age = myreplist$accuage
  gender = myreplist$nsexes
  birthseas = rep(0,4)
  #birthseas had to be specified differently
  birthseas[2] = 1
  
  endgrowt = myreplist$endgrowth
#again birthseason had to be specified differently, correct??
  tmp1 = endgrowt %>% filter(Seas==myreplist$birthseas[1]) %>% select(M, Age_Mat, "Mat*Fecund") %>% mutate(Age_Mat=ifelse(Age_Mat<=0, 0, Age_Mat))
  maa = tmp1$M
  mat = tmp1$Age_Mat
  fec = tmp1$"Mat*Fecund"
#had to add a column of initial ages as missing in SS3.24
  endgrowt$int_Age=rep(c(0:15),8)
  tmp2 = endgrowt %>% select(int_Age, Sex ,Seas, Wt_Beg) %>% pivot_wider(names_from="Seas", values_from="Wt_Beg") %>% arrange(Sex) #%>% select(-"int_Age" ,-"Sex")
  waa_beg = c(tmp2$"1", tmp2$"2", tmp2$"3", tmp2$"4")
  tmp3 = endgrowt %>% select(int_Age, Sex ,Seas, Wt_Mid) %>% pivot_wider(names_from="Seas", values_from="Wt_Mid") %>% arrange(Sex)# %>% select("1","2","3","4")
  waa_mid = c(tmp3$"1", tmp3$"2", tmp3$"3", tmp3$"4")
  
  rm(tmp1,tmp2,tmp3)
  
  return(list(age=age, gender=gender, birthseas=birthseas, waa_beg=waa_beg, waa_mid=waa_mid, mat=mat, fec=fec, maa=maa))
  
}