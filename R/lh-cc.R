ccFn=function(freq,age){
  freq=freq/sum(freq)
  lm  =lm(log(freq)~age)
  hat =exp(predict(lm))
  sel =(freq/hat)/max(freq/hat)
  data.frame(age=age,obs=freq,hat=hat,sel=sel)}
