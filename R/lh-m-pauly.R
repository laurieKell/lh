pauly=function(par,t=10) { #winf,linf="missing",k="misssing",t=10) {
    pauly1=function(par,t)
       exp(-0.2107-0.0824*log(par["winf"])+0.6757*log(par["k"])+0.4627*log(t))
    
    pauly2=function(par,t)
       exp(-0.0066-0.279*log(par["linf"])+0.6543*log(par["k"])+0.4634*log(t))
    
    if ("winf" %in% dimnames(par)$par)
      pauly1(par,t=10)
    else if ("linf" %in% dimnames(par)$par)
      pauly2(par,t=10)}
