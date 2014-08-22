#' vonB
#'
#' Von Bertalanffy growth equation
#' 
#' @param par
#' @param age
#' 
#' #' @export
#' @docType methods
#' @rdname vonB
#' 
#' @seealso \code{\link{invVonB}} \code{\link{gompertz}}  
#' 
#' @examples
#' \dontrun{
#' par=FLPar(linf=100,t0=0,k=.4)
#' age=FLQuant(1:10,dimnames=list(age=1:10))
#' len=vonB(par,age)
#' age=invVonB(par,len)
#' }
vonB=function(par,age){
  res=par["linf"]%*%(1.0-exp((-par["k"])%*%(age%-%par["t0"])))
  dimnames(res)=dimnames(age)
  res}

invVonB=function(par,len)
  -(log(1.0-(len%/%par["linf"]))%/%par["k"])%+%par["t0"]


# setGeneric('vonB', function(params,data, ...)
#   standardGeneric('vonB'))
# setMethod("vonB", signature(params="FLPar",data="ANY"),
#           function(params,data="missing",...) {
#             if (!missing(data) & "FLQuant" %in% is(data) ||  "FLCohort" %in% is(data))
#               data=ages(data)
#             
#             glst[["vonB"]](params,data,...)})
# 
# setGeneric('invVonB', function(params,data, ...)
#   standardGeneric('invVonB'))
# setMethod("invVonB", signature(params="FLPar",data="ANY"),
#           function(params,data="missing",...) {
#             if (!missing(data) & "FLQuant" %in% is(data) ||  "FLCohort" %in% is(data))
#               data=ages(data)
#             
#             glst[["invVonB"]](params,data,...)})
# 
