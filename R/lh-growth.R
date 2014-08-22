################################################################################
# 2) FLQuant/FLCohort to ages ##################################################
# creates FLQuant with ages in cell
setGeneric('ages', function(data, ...)
   standardGeneric('ages'))
setMethod("ages", signature(data="FLQuant"),
   function(data,timing=NULL){
      res<-FLQuant(dimnames(data)$age,dimnames=dimnames(data))

      if (is.null(timing))
         res<-sweep(res,4,(1:dim(res)[4]-1)/dim(res)[4],"+") else
         res<-sweep(res,4,timing,"+")

      return(res)})
setMethod("ages", signature(data="FLCohort"),
   function(data,timing=NULL){
      res<-FLCohort(dimnames(data)$age,dimnames=dimnames(data))

      if (is.null(timing))
         res<-sweep(res,4,(1:dim(res)[4]-1)/dim(res)[4],"+") else
         res<-sweep(res,4,timing,"+")

      return(res)})
################################################################################

# 3) length to weight ##########################################################
## converts wt to len using condition factor
setGeneric('len2wt', function(params,data,...)
  standardGeneric('len2wt'))
  
setMethod("len2wt", signature(params="FLPar", data="FLQuant"),
   function(params,data) params["a"]*data^params["b"])
setMethod("len2wt", signature(params="FLPar", data="FLCohort"),
   function(params,data) params["a"]*data^params["b"])
setMethod("len2wt", signature(params="FLPar", data="numeric"),
   function(params,data) params["a"]*data^params["b"])
################################################################################

# 4) Weight to length ##########################################################
## converts len to wr using condition factor
setGeneric('wt2len', function(params,data, ...)
  standardGeneric('wt2len'))

setMethod("wt2len", signature(params="FLPar", data="FLQuant"),
   function(params,data) (data/params["a"])^(1/params["b"]))
setMethod("wt2len", signature(params="FLPar", data="FLCohort"),
   function(params,data) (data/params["a"])^(1/params["b"]))
setMethod("wt2len", signature(params="FLPar", data="numeric"),
   function(params,data) (data/params["a"])^(1/params["b"]))
################################################################################


