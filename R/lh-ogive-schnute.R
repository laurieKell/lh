schnute<-function(par,age){
  fn1<-function(par,age) (par["y1"]^par["b"]+(par["y2"]^par["b"]-par["y1"]^par["b"])*(1.0-exp(-par["a"]*(age-par["t1"])))/(1.0-exp(-par["a"]*(par["t2"]-par["t1"]))))^(-1/par["b"])
  fn2<-function(par,age)  par["y1"]*exp(log(par["y2"]/par["y1"])*(1.0-exp(-par["a"]*(age-par["t1"])))/(1.0-exp(-par["a"]*(par["t2"]-par["t1"]))))
  fn3<-function(par,age) (par["y1"]^par["b"]+(par["y2"]^par["b"]-par["y1"]^par["b"])*(age-par["t1"])/(par["t2"]-par["t1"]))^(-1/par["b"])
  fn4<-function(par,age)  par["y1"]*exp(log(par["y2"]/par["y1"])*(age-par["t1"])/(par["t2"]-par["t1"]))
  
  if (par["a"]!=0 & par["b"]!=0) return(fn1(par,age))
  if (par["a"]!=0 & par["b"]==0) return(fn2(par,age))
  if (par["a"]==0 & par["b"]!=0) return(fn3(par,age))
  if (par["a"]==0 & par["b"]==0) return(fn4(par,age))}
