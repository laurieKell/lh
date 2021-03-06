#' Gislason
#'
#' Chen and Watanabe natural mortality equation
#' 
#' @param par FLPar with von Bertlanffy parameters \code{t0, linf, k} 
#' @param len for which M to be predicted
#' 
#' #' @export
#' @docType methods
#' @rdname gislason
#' 
#' @seealso \code{\link{lorenzen}}
#' 
#' @examples
#' \dontrun{
#' par=FLPar(linf=100,t0=-0.1,k=.4)
#' age=FLQuant(1:20,dimnames=list(age=1:20))
#' m  =chenWatanabe(par,age)
#' }
gislason=function(x,par) #(l,linf,k) 
   exp(0.55-1.61*log(x) %+% 1.44*log(params["linf"]) %+% log(params["k"]))

gislason=function(x,par) #(l,linf,k) 
   exp(0.6590-(1.6911*log(x))%+%(1.4440*log(params["linf"]))%+%(0.8976*log(params["k"])))

