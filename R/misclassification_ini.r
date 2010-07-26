###############################
###############################
##  gives misclassified samples on initial and final random forest 
##  Input: NA
##  Output: NA
###############################
###############################
"misclassification_ini"<-function(){
load(file=paste(strsplit(filename.RF,".Rdata")[[1]],".iniSRF",sep=""))
SRF_ini_dat$fact.temp==predict(SRF_ini)
rm(SRF_ini)
gc(reset=TRUE)
load(file=paste(strsplit(filename.RF,".Rdata")[[1]],".fitSRF",sep=""))
SRF_ini_dat_fitted$fact.temp==predict(SRF_ini_fitted)
rm(SRF_ini_fitted)
gc(reset=TRUE)
}