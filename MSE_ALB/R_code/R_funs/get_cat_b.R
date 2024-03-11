#Function to extract Total catch weight from SS output
#Created by H. Ijima for NPALB projection software
get_cat_b = function(myreplist) {
  
  tmp1 = myreplist$sprseries
  tmp2 = tmp1[,-c(2,3)]
  cat_b = tmp2 %>% select(Yr,Retain_Catch_B)
  return(cat_b)
  
  rm(tmp1,tmp2)
  
}