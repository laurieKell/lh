richardsCapped <- function(par,age) { #x, a50, ato95, sigma, amax) {
  beta <- ato95*log(19)/(log(2^sigma-1)-log((20/19)^sigma-1))
  alpha <- a50+beta*log(2^sigma-1)/log(19)
  
  return((amax/(1+19^(alpha-x)/beta))^1/sigma)}

