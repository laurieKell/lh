# mlst=list("gunderson"         =gundersonDygert,
#           "pauly"             =pauly,
#           "hoenig"            =hoenig,
#           "jensen"            =jensen,
#           "richter"           =richterEfanov,
#           "peterson"          =petersonWroblewski,
#           "lorenzen"          =lorenzen,
#           "mcgurk"            =mcgurk,
#           "gislason"          =gislason,
#           "chen"              =chenWatanabe)
# 
# rm(list=names(mlst))
# 
# setGeneric('mFn', function(model,params,data, ...)
#    standardGeneric('mFn'))
# setMethod("mFn", signature(model="character",params="FLPar",data="ANY"),
#    function(model,params,data="missing",...) {
#      if (!missing(data) & "FLQuant" %in% is(data) ||  "FLCohort" %in% is(data))
#           data=ages(data)
#          
#       mlst[[model]](params,data,...)})

# par=FLPar(linf=100,k=0.2,t0=0,amax=40,gsi=0.2)
# mFn("hoenig",   par)
# mFn("gunderson",par)
# mFn("pauly",    par)
# mFn("jensen",   par)
# mFn("richter",  par)
# mFn("peterson", par)
# mFn("lorenzen", par)
# mFn("mcgurk",   par)
# mFn("gislason", par)
# mFn("chen",     par)
         