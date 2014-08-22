logisticProduct <- function(params,data) { #x,a50,ato95,b50,bto95,amax=1.0){
  func <- function(x,a50,ato95,b50,bto95,amax){
    if (ato95 < 0.02 && bto95 < 0.02)
    {
      if (a50 <= x && x <= (a50+b50))
        return(amax)
      else
        return(0)
    } else if (ato95 < 0.02) {
      funcMax = 1+pow(19.0,(-b50)/bto95)
      return(amax * funcMax * (1/(1+pow(19.0,(x-(a50+b50))/bto95))))
    } else if (bto95 < 0.02) {
      funcMax = 1+pow(19.0,(-b50)/ato95)
      return(amax * funcMax * (1/(1+pow(19.0,(a50-x)/ato95))))
    } else {
      funcMax = 0
      for (i in 0:100) {
        tempvar = a50 - ato95 + i * (b50 + bto95 + ato95) / 100
        funcMax = max(funcMax, (1+pow(19.0,(a50-tempvar)/ato95))*
                        (1+pow(19.0,(tempvar-(a50+b50))/bto95)))
      }
      return(amax * funcMax * (1/((1+pow(19.0,(a50-x)/ato95))
                                  * (1+pow(19.0,(x-(a50+b50))/bto95)))))
    }
  }    
  sapply(x,func,a50,ato95,b50,bto95,amax)}


