#Function to extract catch at age from SS output
#created by H. Ijima for the NPALB Projection software
get_caa = function(myreplist) {
  
  startyr = myreplist$startyr
  
  catage = myreplist$catage
  tmp = catage[,-c(1,2,4,5,6,9,10)]
  tmp = tmp %>% filter(Yr>=startyr) %>% group_by(Yr,Gender,Seas) %>% summarise_all(funs(sum))
  tmp = reshape2::melt(tmp, id=c("Yr","Gender","Seas"))
  caa = tmp %>% rename(age=variable,cat.n=value) %>% arrange(Gender,Seas)
  
  rm(tmp)
  
  return(caa=caa)
  
}
