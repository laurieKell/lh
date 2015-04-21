dirMy ='/home/laurie/MEGAsync/era1'
dirInp=paste(dirMy,'inputs',sep="/")
dirDat=paste(dirMy,'data',  sep="/")

dat=read.csv("/home/laurie/MEGAsync/era1/inputs/ALLSpNONSTIdata.csv",sep=";")
dat=dat[,sort(names(dat))]
nmOrg=c("A50",   "Adult",  "Amax",   "Avail", "CAT.OCE", "cien..Name",
        "Code",  "Encout", "Fec",    "F.M",   "K",       "L50",     
        "L50.Lmax",  
        "Linf",  "Lmax",   "M",      "Manag..Strategy",  "Mort",      
        "Ocean", "Sp",     "SYM.Fam","Value")

nmNew=c("a50",   "adult", "amax",  "avail",  "cat",   "name",    
        "code",  "encout","fec",   "fm",     "k",      "l50",   
        "l50lmax",
        "linf",  "lmax",  "m",     "manage", "mort",  
        "ocean", "sp",    "family","value")
dat=dat[,nmOrg]
names(dat)=nmNew

quant =c("lmax","l50","adult","l50lmax","fec",
         "linf","k","a50","amax","m","mort")
qual  =c("avail","encout")
cat   =c("sp","name","family","ocean","manage","value")
dup   =c("cat","code")
quant1=c("lmax","l50","l50lmax","fec","linf","k","a50","amax","m")
quant2=c("adult","mort")

dat=dat[,c(cat,quant,qual)]
dat$manag =tolower(dat$manag)
dat$encout=tolower(dat$encout)
dat$avail =tolower(dat$avail)
dat$fam   =tolower(dat$fam)

dat=transform(dat,
              value=factor(value,levels=c("T","BY/KA","BY/KC","BY/D"),
                           labels=c("Target","Eaten Onboard",
                                    "Retained","Discarded")),
              ocean=factor(ocean,levels=c("ATL","IND"),
                           labels=c("Indian","Atlantic")))
save(dat,file=paste(dirDat,"dat.RData",sep="/"))

lhPel=dat[,c(quant1,"ocean")]
save(lhPel,file="/home/laurie/Desktop/flr/git/lh/data/lhPel.RData")
