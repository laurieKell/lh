#' gompertz
#'
#' Gompertz growth equation
#' 
#' @param par FLPar with parameters for \code{linf, a, b}
#' @param age
#' 
#' #' @export
#' @docType methods
#' @rdname gompertz
#' 
#' @seealso \code{\link{vonB}}   
#' 
#' @examples
#' \dontrun{
#' par=FLPar(linf=100,t0=0,k=.4)
#' age=FLQuant(1:10,dimnames=list(age=1:10))
#' len=gompertz(par,age)
#' }
gompertz=function(par, age) 
   par["linf"]%*%exp(-par["a"]%*%par["b"]%^%age)


