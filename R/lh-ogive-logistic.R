#' logistic
#'
#' logistic function
#' 
#' @param par
#' @param age
#' 
#' #' @export
#' @docType methods
#' @rdname logistic
#' 
#' @seealso \code{\code{\link{gompertz}}}
#' 
#' @examples
#' \dontrun{
#' par=FLPar(linf=100,t0=0,k=.4)
#' age=FLQuant(1:10,dimnames=list(age=1:10))
#' mat=logistic(par,age)
#' }
logistic <- function(par,age) { #x, a50, ato95){
  func <- function(x,a50,ato95,asym){
    if ((a50-x)/ato95 > 5)
      return(0)
    if ((a50-x)/ato95 < -5)
      return(asym)
    return(asym/(1.0+pow(19.0,(a50-x)/ato95)))}
  
  sapply(age,func,par["a50"],par["ato95"],par["asym"])
} 

pow<-function(a,b) a^b
logisticFn<-function(params,data) { #x,a50,ato95,asym=1.0){  
  
  res =params["asym"]%/%(1.0+pow(19.0,(params["a50"]%-%data)%/%params["ato95"]))
  res[is.na(res)]=0
  asym=FLQuant(1,dimnames=dimnames(data))%*%params["asym"]
  grt =(params["a50"]%-%data)%/%params["ato95"] >  5
  lss =(params["a50"]%-%data)%/%params["ato95"] < -5
  
  res[grt]=0
  res[lss]=asym[lss]
  
  dmns          =dimnames(res)
  names(dmns)[1]="age"
  dimnames(res) =dmns
  
  return(res)}
