logisticDouble <- function(params,data) { #x, a50, ato95, b50, bto95, amax=1.0){
  
  func <- function(x,a50,ato95,b50,bto95,amax){
    if (ato95 < 0.02 && bto95 < 0.02)
    {
      if (a50 <= x && x <= (a50+b50)) return(amax) else return(0)
    } else if (ato95 < 0.02) {
      p = a50
      funcMax = 1+pow(19.0,(p-(a50+b50))/bto95)
      return(amax * funcMax * (1+pow(19.0,(x-(a50+b50))/bto95)))
    } else if (bto95 < 0.02) {
      p = a50+b50
      funcMax = 1+pow(19.0,(a50-p)/ato95)
      return(amax * funcMax * (1+pow(19.0,(a50-x)/ato95)))
    } else {
      p = (a50 * bto95 + ato95 * (a50 + b50)) / (ato95 + bto95)
      funcMax = 1+pow(19.0,(a50-p)/ato95)
      return(amax * funcMax * min(1/(1+pow(19.0,(a50-x)/ato95)),
                                  1/(1+pow(19.0,(x-(a50+b50))/bto95))))
    }}
  
  sapply(x,func,a50,ato95,b50,bto95,amax)} 