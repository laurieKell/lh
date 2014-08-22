#' chenWatanabe
#'
#' Chen and Watanabe natural mortality equation
#' 
#' @param par FLPar with von Bertlanffy parameters \code{t0, linf, k} 
#' @param age for which growth to be predicted
#' 
#' #' @export
#' @docType methods
#' @rdname vonB
#' 
#' @seealso \code{\link{invVonB}} \code{\link{gompertz}}  
#' 
#' @examples
#' \dontrun{
#' par=FLPar(linf=100,t0=-0.1,k=.4)
#' age=FLQuant(1:20,dimnames=list(age=1:20))
#' m  =chenWatanabe(par,age)
#' }
chenWatanabe=function(par,age) {
  m =par["k"]%/%(1-exp(-par["k"]%*%(age-par["t0"])))
  
  tm =((1/par["k"])%*%log(1-exp(par["k"]%*%par["t0"]))) #%+%par["t0"]
  dimnames(tm)=dimnames(par["t0"])
  tm=-tm%+%par["t0"]
  ## bug as %-% is %*%
  bit=exp(-par["k"]%*%(tm-par["t0"]))
  print(tm-par["t0"])
  print(tm%-%par["t0"])
  
  a0=1-bit
  a1=par["k"]%*%bit
  a2=FLPar(-0.5%*%exp(log(par["k"])%*%2)%*%bit,dimnames=dimnames(a1))
  dimnames(a2)=dimnames(a1)
  age.=age>c(tm)
  #m[age.] =par["k"]/(a0+a1*(age[age.]-tm)+a2*(age[age.]-tm)^2)
#   m[age.] =par["k"]%/%(a0%+%(a1%*%(age[age.]%-%tm))%+%
#                       (a2%*%(age[age.]%-%tm)^2))
z2.<-((a2%*%((age[age.]%-%tm)^2)))
z1.<-(a0%+%(a1%*%(age[age.]%-%tm)))

  m[age.] =par["k"]%/%(z1.%+%z2.)
  
  m[m<0]=max(m)
  
  return(m)}   

chenWatanabeFn=function(par,age) { #(age,k,t0=-0.1){
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
