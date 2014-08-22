dnormalPlateau <- function(x, a1, a2, amax, sL, sR) {
  if (x<=a1) 
    return(amax*2^-((x-a1)/sL)^2)
  else if (a1<x & x<=(a1+a2))
    return(amax*2^-((x-a1)/sL)^2)
  else
    return(amax*2^-((x-(a1+a2))/sR)^2)}


