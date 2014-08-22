dnormalColeraine <- function(x, a, b, c) {
  if (x<a)
    return(exp(-(x-a)^2/b^2))
  else
    return((-(x-a)^2/c^2))}



