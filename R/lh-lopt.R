loptFn=function(x,params,age=0:200,
                fnM=function(par,len) 
                    0.55*(len^-1.66)%*%(par["linf"]^1.44)%*%par["k"]){
    
  len   =vonB(params,FLQuant(age,dimnames=list(age=age)))
  m     =fnM(params,len)
  mCum  =FLQuant(aaply(m,2,cumsum),dimnames=dimnames(m))
  
  a =max(0,invVonB(params,x))

  a_=floor(a)
  
  n =exp(-mCum[ac(a_)]-m[ac(a_)]*(a-a_))
  c(n*len2wt(params,x))}

#params=par[,10]
#params["t0"]=-.1
#loptFn(x,params)

lopt=function(params,
              fnM=function(par,len) 
                    0.55*(len^-1.66)%*%(par["linf"]^1.44)%*%par["k"])
    optimise(loptFn,c(.01,c(params["linf"])*.99),par=params,maximum=TRUE,fnM=fnM)$maximum
