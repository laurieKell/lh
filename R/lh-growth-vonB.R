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
#' @seealso \code{\code{\link{gompertz}}}  
#' 
#' @examples
#' \dontrun{
#' par=FLPar(linf=100,t0=0,k=.4)
#' age=FLQuant(1:10,dimnames=list(age=1:10))
#' len=vonB(age,par)
#' age=vonB(par,length=len)
#' }
vonBFn=function(x,par){
  res=par["linf"]%*%(1.0-exp((-par["k"])%*%(x%-%par["t0"])))
  
  dimnames(res)=dimnames(x)
  res}

invVonBFn=function(x,par){
  res=log(1-(x%/%par["linf"]))%/% (-par["k"])%+%par["t0"]

  dimnames(res)=dimnames(x)
  res}

setGeneric('vonB', function(x,par,...)
  standardGeneric('vonB'))
setMethod("vonB", signature(x="numeric",par="numeric"),
          function(x,par,...) 
            vonBFn(x,par))
setMethod("vonB", signature(x="FLQuant",par="numeric"),
          function(x,par,...) { 
            res=vonBFn(x,FLPar(par))
            units(res)=""
            res})
setMethod("vonB", signature(x="FLQuant",par="FLPar"),
          function(x,par,...){   
            res=vonBFn(x,par)
            units(res)=""
            res})
setMethod("vonB", signature(x="FLPar",par="missing"),
          function(x,par,length,...){   
            res=invVonBFn(x=length,par=x)
            units(res)=""
            res})
setMethod("vonB", signature(x="FLPar",par="FLPar"),
          function(x,par,...){   
            res=vonBFn(x,par)
            units(res)=""
            res})
# library(numDeriv)
# par=FLPar(linf=318.9,k=0.093,t0=-0.970)
  # fnL=function(len) invVonB(par,FLQuant(len))
  # fnA=function(age)    vonB(par,FLQuant(age,dimnames=list(age=age)))
  # 
  # grad(fnL,fnA(15))


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
