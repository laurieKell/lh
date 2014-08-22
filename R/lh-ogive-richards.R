richards <- function(par,age) { #x, a50, ato95, sigma) {
  beta <- ato95*log(19)/(log(2^sigma-1)-log((20/19)^sigma-1))
  alpha <- a50+beta*log(2^sigma-1)/log(19)
  
  return((1/(1+19^(alpha-x)/beta))^1/sigma)} 