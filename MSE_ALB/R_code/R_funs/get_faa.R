#Function to caculate fishing mortality at age
#Created by H. Ijima for NPALB projection software and modified by D. Tommasi to integrate age-varying M
get_faa = function(naa,caa,myreplist) {
  
  startyr = myreplist$startyr 
  
  tmp0 = naa %>% filter(Seas==2) %>% mutate(age=as.numeric(age))
  names(tmp0)[2]="Gender"
  tmp1 = caa %>% filter(Seas==1) %>% select(Yr,Gender,age,cat.n) %>% mutate(Yr=Yr-1)
  tmp2 = caa %>% filter(Seas>=2) %>% group_by(Yr,Gender,age) %>% summarise(cat.n=sum(cat.n))
  tmp3 = bind_rows(tmp1,tmp2)
  tmp3 = tmp3 %>% group_by(Yr,Gender,age) %>% summarise(cat.n=sum(cat.n)) %>% mutate(age=as.numeric(age))
  tmp4 = full_join(tmp0,tmp3,by=c("Yr","Gender","age")) %>% filter(Yr>=startyr )
  
  #find out if any catches are greater than the numbers
  id4 = which(tmp4$pop_n<=tmp4$cat.n)
  #make a new column and set those to 0
  tmp4$cat2 = tmp4$cat.n
  tmp4$cat2[id4]=0
  
  #faa was calculated by Newton method
  x = list()
  for (i in 1:length(tmp4$Yr)) {
    fx = function (x) (x/(x+tmp4$M[i]))*(1-exp(-x-tmp4$M[i]))*tmp4[i,5]-tmp4[i,8]
    res = uniroot(fx,c(0,200))
    x[i] = res$root
  }
  
  tmp4$faa = unlist(x)
  
  #set the f fo rwhich caa was set to 0 to 1
  tmp4$faa[id4]=1
  #find faa >2 and set them to 2
  tmp4$faa[which(tmp4$faa>2)]=2
  
  #rm(tmp0,tmp1,tmp2,tmp3)
  
  return(tmp4)
  
}