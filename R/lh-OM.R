#' om
#' 
#' Uses life history theory to derive parameters for biological relationships,
#' i.e. growth, maturity, natural mortality from.
#' 
#'
#' @param   \code{par} \code{FLPar} object with parameters for life history equations and selection pattern.
#' Need L_infty to estimate other parameters, if any other parameters supplied in \code{code} then
#' these are not provided by the algorithm 
#' @param   \code{t0} of von Bertalanffy. This is a default that isnt normally derived
#' from life history theory, as are the following args.
#' @param   \code{a} coefficient of length weight relationship
#' @param   \code{b} exponent of length weight relationship
#' @param   \code{ato95} age at which 95\% of fish are mature, offset to age at which 50\% are mature
#' @param   \code{sl} selectivity-at-age parameter, standard deviation of lefthand limb of double normal
#' @param   \code{sr} stock recruitment relationship
#' @param   \code{s} steepness of stock recruitment relationship
#' @param   \code{v} virgin biomass
#' 
#' @export
#' @docType methods
#' @rdname lh
#' @return An \code{FLPar} object with parameters
#' @examples
#' \dontrun{
#' 
omFn=function(x,b0=.8,b=.1,year=seq(40,90),maxF=5){
  
  # get reference pointe
  #x  =eql
  x  =brp(x)
  rf =refpts(x)
  dms=dimnames(rf)
  
  # set initial biomass
  dms$refpt=c("initial","final",dms$refpt)
  rf=FLPar(NA,dimnames=dms)
  rf["initial","ssb"]=refpts(x)["virgin","ssb"]*b0
  rf["final",  "ssb"]=refpts(x)["virgin","ssb"]*b
  
  refpts(x)=rf
  refpts(x)=computeRefpts(x)
  
  fbar=FLQuant(NA,dimnames=list(year=seq(max(year)),iter=seq(dims(x)$iter)))
  fbar[,1:min(year)][]=rep(c(refpts(x)["initial","harvest"]), each=length(1:min(year)))
  
  fbar[,year][]=
    t(maply(data.frame(i=seq(dim(rf)[3])),
            function(i) 
              seq(refpts(x)["initial","harvest",i],
                  refpts(x)["final",  "harvest",i],
                  length.out=length(year))))  
  fbar(x)=fbar
  
  res=fwd(x,maxF=maxF)
  res}

if (FALSE){
library(ggplot2)
library(FLBRP)
library(lh)

data(lhPel)
data(imPel)
head(lhPel)

load("/home/laurie/MEGAsync/productivity/data/imPel.RData")

 mf2FLPar=function(x){
    dmns=dimnames(x)[2:1]
    names(dmns)=c("params","iter")
    x=t(as.matrix(x))
 
    FLPar(array(x,dim=dim(x),dimnames=dmns),units="")}
 
 imPel=mf2FLPar(cbind(ato95=0,sl=0,imPel[,c("linf","l50","k","m")]))
 imPel=gislasim(imPel)

 eql=lh(imPel[,!is.na(c(imPel["a50"])),],
        fnM=function(par,len) {
        len[]=rep(par["m"],each=dim(len)[1])
                 len},
        growth=lh:::vonB,
        spwn=0.6)


om=omFn(eql)
om=fwdWindow(om,end=109,eql)

srDev=rlnorm(100,FLQuant(0,dimnames=list(year=1:109)),.5)

f=fbar(om)[,-1]
f[,ac(90:109)]=
  FLQuant(seq(1,.01,length.out=20),dimnames=list(year=90:109))%*%fbar(om)[,90]
save(om,f,srDev,file="/home/laurie/MEGAsync/productivity/data/om.RData")

i=1
f. =iter(f,  i)
dimnames(f.)$iter=1
om1=fwd(iter(om, i), 
        f =f.,
        sr=iter(eql,i),
        sr.residuals=srDev,
        maxF=5)

plot(om1)


#oem
rmultinom(50, size=1000, prob=seq(1/50,50))
}

