dnormalCapped <- function(params,data){ #x,a,sL,sR,amax=1.0) {
  func<-function(x,a,sL,sR,amax) {
    if (x < a)
      return(amax*pow(2.0,-((x-a)/sL*(x-a)/sL)))
    else
      return(amax*pow(2.0,-((x-a)/sR*(x-a)/sR)))
  }
  
  sapply(data,func,params["a"],params["sl"],params["sr"],params["amax"])}


