knife=function(par, age){
  res=age
  
  res[age> c(par["break"])][]=par["m1"]
  res[age<=c(par["break"])][]=par["m2"]
  
  return(res)}


