dnormal <- function(params,data){
  pow <-function(a,b) a^b
  func<- function(data,a1,sl,sr){
    if (data < a1)
      return(pow(2.0,-((data-a1)/sl*(data-a1)/sl)))
    else
      return(pow(2.0,-((data-a1)/sr*(data-a1)/sr)))}
  
  sapply(data,func,params["a1"],params["sl"],params["sr"])}


dnormal <- function(par,age){
  
  a1=FLQuant(1,dimnames=dimnames(age))%*%par["a1"]
  s =FLQuant(1,dimnames=dimnames(age))%*%par["sl"]
  sr=FLQuant(1,dimnames=dimnames(age))%*%par["sr"]
  
  if (dims(age)$iter==1 &  dims(a1)$iter>1)
    data=propagate(age,dims(a1)$iter)
  
  s[age>=a1]=sr[age>=a1]
  
  res=2.0^(-((age%-%a1)%/%s%*%(age%-%a1)%/%s))
}
