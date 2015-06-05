test_that("Testing M functions", {
  
  library(lh)
  
  #California sheephead (Semicossyphus pulcher)
  par =FLPar(linf=79.28,k=0.0683,t0=-0.0,a=2.6935e-3,b=2.857)
  mass=c(1.90,4.23,7.47,11.48,16.04,20.96,26.07,31.22,36.28,41.17,45.83,50.20,54.27,58.03,61.48,64.62)
  age =FLQuant(1:16,dimnames=list(age=1:16))
  len =(mass/par["a"])^(1/par["b"])
  
  fn<-function(x){
    mass=c(1.90,4.23,7.47,11.48,16.04,20.96,26.07,31.22,36.28,41.17,45.83,50.20,54.27,58.03,61.48,64.62)
    age =FLQuant(1:16,dimnames=list(age=1:16))
    res =(vonB(age,FLPar(linf=79.28,k=0.068,t0=x[2]))^par["b"])*par["a"]
    
    sum((res-mass)^2)}
  
  res=optim(c(0.068,-0.3),fn)
  
  par.=FLPar(linf=79.28,k=res$par[1],t0=res$par[2])
  hat =par["a"]*(vonB(age,par)^par["b"])
  
  test=data.frame(
    age     =1:16,
    len     =(mass/par["a"])^(1/par["b"]),
    mass    =mass,
    hoenig  =0.27725,
    jensen  =c(0.33,rep(0.195,15)),
    rickterE=0.32,
    lorenzen=c(0.25,0.20,0.17,0.15,0.13,0.12,0.12,0.11,0.11,0.10,0.10,0.10,0.09,0.09,0.09,0.09),
    peterW  =c(0.16,0.13,0.12,0.10,0.10,0.09,0.08,0.08,0.08,0.08,0.07,0.07,0.07,0.07,0.07,0.07),
    mcgurk  =c(0.41,0.31,0.25,0.21,0.19,0.17,0.16,0.15,0.14,0.13,0.13,0.12,0.12,0.12,0.11,0.11),
    chenW   =c(0.43,0.34,0.28,0.25,0.22,0.20,0.19,0.18,0.17,0.17,0.18,0.20,0.22,0.24,0.25,0.26))
  
  rm(mass)
  
  ## Lorenzen
  #numeric
  expect_equal(2.6935e-5*(test$len^2.857), test$mass)  
  
  test$lorenzen=lorenzen(test$mass)
  expect_equal(test$lorenzen, lorenzen(test$mass))
  expect_equal(test$lorenzen, lorenzen(test$mass,par=c(a=.3,b=-0.288)))
  
  #FLQuant
  m   =FLQuant(test$lorenzen,dimnames=list(age=test$age),units="yr^-1")
  mass=FLQuant(test$mass,    dimnames=list(age=test$age),units="yr^-1")
  
  expect_equal(m, lorenzen(mass))
  expect_equal(m, lorenzen(mass,par=c(a=.3,b=-0.288)))
  expect_equal(m, lorenzen(mass,par=FLPar(a=.3,b=-0.288)))
  
  m=propagate(m,10)
  expect_equal(m, lorenzen(mass,par=propagate(FLPar(a=.3,b=-0.288),10)))
  expect_equal(m, lorenzen(propagate(mass,10),par=FLPar(a=.3,b=-0.288)))

  ## Chen Watanabe
  #numeric
  par=c(linf=150,k=0.3,t0=-0.3)
  test$chenW=chenW(test$age,par)
  expect_equal(test$chenW, chenW(test$mass,par))
  
  #FLQuant
  m  =FLQuant(test$chenW,dimnames=list(age=test$age),units="yr^-1")
  age=FLQuant(test$age,  dimnames=list(age=test$age),units="yr^-1")
  
  expect_equal(m, chenW(age,par))
  expect_equal(m, chenW(age,FLPar(par)))
  
  m=propagate(m,10)
  expect_equal(m, chenW(age,par=propagate(FLPar(par),10)))
  expect_equal(m, chenW(propagate(age,10),par=FLPar(par)))
  })
