#' chenW
#'
#' Chen and Watanabe natural mortality equation
#' provides an estimate of M per year at age as a function of von Bertalanffy growth equation
#' 
#' @param age  age at which M is to be predicted
#' @param par FLPar or numeric vector  with von Bertlanffy parameters \code{t0, linf, k} 
#' 
#' #' @export
#' @docType methods
#' @rdname chenW
#' 
#' @seealso \code{\link{chenW},\link{gislason}}
#'  
#' @examples
#' \dontrun{
#' par=FLPar(linf=200,k=.5,t0=-.3)
#' chenW(1:10,par)
#' }

chenWFn=function(age,par) {
  m =par["k"]%/%(1-exp(-par["k"]%*%(age-par["t0"])))
  
  tm =((1/par["k"])%*%log(1-exp(par["k"]%*%par["t0"]))) #%+%par["t0"]
  dimnames(tm)=dimnames(par["t0"])
  tm=-tm%+%par["t0"]
  ## bug as %-% is %*%
  bit=exp(-par["k"]%*%(tm-par["t0"]))
#  print(tm-par["t0"])
#  print(tm%-%par["t0"])
  
  a0=1-bit
  a1=par["k"]%*%bit
  a2=FLPar(-0.5*exp(log(par["k"])*2)%*%bit,dimnames=dimnames(a1))
  dimnames(a2)=dimnames(a1)
  age.=age>c(tm)
  #m[age.] =par["k"]/(a0+a1*(age[age.]-tm)+a2*(age[age.]-tm)^2)
#   m[age.] =par["k"]%/%(a0%+%(a1%*%(age[age.]%-%tm))%+%
#                       (a2%*%(age[age.]%-%tm)^2))

  if (any(age.)){
    z2.<-((a2%*%((age[age.]%-%tm)^2)))
    z1.<-(a0%+%(a1%*%(age[age.]%-%tm)))
  
    m[age.] =par["k"]%/%(z1.%+%z2.)
    }

  m[m<0]=max(m)
  
  return(m)}   

chenWFn2=function(age,par) { #(age,k,t0=-0.1){
  m =par["k"]/(1-exp(-par["k"]*(age-par["t0"])))
  
  tm =-(1/par["k"])*log(1-exp(par["k"]*par["t0"]))+par["t0"]
  bit=exp(-par["k"]*(tm-par["t0"]))

  a0=1-bit
  a1=par["k"]*bit
  a2=-0.5*par["k"]^2*bit
  age.=age>c(tm)

  m[age.] =par["k"]/(a0+a1*(age[age.]-tm)+a2*(age[age.]-tm)^2)
  
  m[m<0]=max(m)
  
  return(m)}   

setGeneric('chenW', function(age,par,...)
  standardGeneric('chenW'))
setMethod("chenW", signature(age="numeric",par="numeric"),
          function(age,par,...) 
            chenWFn2(age,par))
setMethod("chenW", signature(age="FLQuant",par="numeric"),
          function(age,par,...) { 
            res=chenWFn(age,FLPar(par))
            units(res)="yr^-1"
            res})
setMethod("chenW", signature(age="FLQuant",par="FLPar"),
          function(age,par,...){   
            res=chenWFn(age,par)
            units(res)="yr^-1"
            res})
