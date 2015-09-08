a50=function(params,data) {
  a50=FLQuant(ceiling(rep(c(params["a50"]),each=dim(data)[1])),
              dimnames=dimnames(data))
  res=FLQuant(0.5,dimnames=dimnames(data))
  res[data> a50]=1
  res[data< a50]=0
  
  res}