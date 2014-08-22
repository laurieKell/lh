#' jensen
#'
#' Jensen natural mortality estimate based on k of the von Bertalanffy growth equantion
#' 
#' @param par FLPar with von Bertlanffy parameter \code{k} 
#' 
#' #' @export
#' @docType methods
#' @rdname jensen
#' 
#' @seealso \code{\link{lorenzen}}  
#' 
#' @examples
#' \dontrun{
#' jensen(FLPar(k=.5)
#' }
jensen=function(params,data=NULL)
   1.5*params["k"]
