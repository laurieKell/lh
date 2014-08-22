#' lorenzen
#'
#' Lorenzen natural mortality equation that provides an estimate of 
#' M the natural mortality at weight as a function of M at unit weight
#' and b a scaling factor. 
#' Natural mortality is measured as an annual rate
#' 
#' @param wt  weight at which M is to be predicted
#' @param a M at unit weight, default = 0.2
#' @param b scaling factor, default = -0.288
#' 
#' #' @export
#' @docType methods
#' @rdname lorenzen
#' 
#' @seealso \code{\link{chenWatanabe}}
#'  
#' @examples
#' \dontrun{
#' wt =FLQuant(seq(1,10,1),dimnames=list(age=1:10))
#' lorenzen(wt)
#' }
 lorenzen=function(wt,a=0.2,b=-0.288) 
   a*wt^b

