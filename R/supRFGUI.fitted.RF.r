###############################
###############################
##  Build supervised random forest model with to sufficient probes
##  Input: NA
##  Output: NA
###############################
###############################
"supRFGUI.fitted.RF"<-function(){
dev_delete()
prediction<-function(){
print("Complete error table made by the supervised random forest model with to sufficient probes")
print(SRF_fit_prediction[[1]])
print("Confusion matrix made by the supervised random forest model with to sufficient probes")
print(SRF_fit_prediction[[2]])
print("Out-of-bag of the supervised random forest model with to sufficient probes (% of misclassification)")
print(SRF_fit_prediction[[3]])
}
RF_temp<-function(){
choiceGUI("Supervised random forest model with to sufficient probes","What is your categorical variable ?",paste(colnames(fact.RF),collapse=","))
fact.temp<<-fact.RF[,nf]
fact.supRF.fitted<<-colnames(fact.RF)[nf]
choiceGUI("Supervised random forest model with to sufficient probes","How many forests ?","10 advised")
SRF_ini_nbforest_fitted<<-nf
choiceGUI("Supervised random forest model with to sufficient probes","How many trees per forest","10000 is advised")
nbtree.temp<<-nf
SRF.fit.nbtree<<-nbtree.temp#

if(exists("SRF_ini_mtry_fitted")) choiceGUI("Supervised random forest","What is the mtry ?",paste("Please see the result with the 'Best mtry determination' button; otherwise heuristic value",round(sqrt(ncol(mat.analyse.RF.fitted)))))
if(!exists("SRF_ini_mtry_fitted"))choiceGUI("Supervised random forest","What is the mtry ?",paste("Heuristic formula advises",round(sqrt(ncol(mat.analyse.RF.fitted))),"but the 'Best mtry determination' button provides better results."))



mtry.temp<<-nf
SRF.fit.mtry<<-mtry.temp#
library("party")
mycontrols <- cforest_unbiased(ntree=nbtree.temp, mtry=mtry.temp, minsplit=5)
mycontrols <<-mycontrols
set.seed(2908)
dat<-data.frame(mat.analyse.RF.fitted,fact.temp)
SRF_ini_dat_fitted<<-dat
print("Pleased wait while working")
SRF_ini_OOB_fitted<<-1
for (i in 1:SRF_ini_nbforest_fitted){
waitGUI(i,SRF_ini_nbforest_fitted)
if(exists("SRF_ini_fitted")){rm(SRF_ini_fitted);gc(reset=TRUE) } #
SRF_ini_fitted<- cforest(formula(paste(names(dat)[length(dat)],"~.")), data=dat, controls=mycontrols)
OOB<<-c(1-c(sum(SRF_ini_dat_fitted$fact.temp==predict(SRF_ini_fitted))/nrow(SRF_ini_dat_fitted)))#% of correct prediction <=> OOB estimate of  error rate
print(paste("0ut-of-bag of iteration",i,"is of",OOB))
if (OOB<SRF_ini_OOB_fitted){
##save
if(exists("filename.RF")){  ##si existe et path actuel=path du projet	
if(paste(strsplit(filename.RF,"/")[[1]][c(1:c(length(strsplit(filename.RF,"/")[[1]])-1))],collapse="/")!=getwd()) {
filename.RF<-paste(getwd(),strsplit(filename.RF,"/")[[1]][c(c(length(strsplit(filename.RF,"/")[[1]])))],sep="/")
filename.RF<<-filename.RF}
}
if(!exists("filename.RF")){   #si existe pas
filename.RF<-tclvalue(tkgetSaveFile())
filename.RF<-paste(filename.RF,".Rdata",sep="")
if (filename.RF == "")
return()
filename.RF<<-filename.RF
}
SRF_ini_OOB_fitted<<-OOB
SRF.fitted.prox.raw<<-proximity(SRF_ini_fitted)
aa<-data.frame(SRF_ini_dat_fitted$fact.temp==predict(SRF_ini_fitted),SRF_ini_dat_fitted$fact.temp,predict(SRF_ini_fitted))
colnames(aa)<-c("Prediction","Real factor","Predicted factor")
rownames(aa)<-rownames(SRF_ini_dat_fitted)
complete_table<-aa
conf<-table(SRF_ini_dat_fitted$fact.temp,predict(SRF_ini_fitted))
confusion_matrix<-conf
SRF_fit_prediction<<-list(complete_table,confusion_matrix,SRF_ini_OOB_fitted)
save(SRF_ini_fitted,file=paste(strsplit(filename.RF,".Rdata")[[1]],".temp",sep=""))
best<<-i
}
}
##save
if(length(which(paste(getwd(),list.files(getwd()),sep="/")==paste(strsplit(filename.RF,".Rdata")[[1]],".fitSRF",sep="")))==1){
choiceGUI("Save","Replace existing file","1:no, 2:yes, 3:cancel")
if(nf==1) return()
if(nf==2) {
file.rename(from=paste(strsplit(filename.RF,".Rdata")[[1]],".temp",sep=""), to=paste(strsplit(filename.RF,".Rdata")[[1]],".fitSRF",sep=""))
print("successfuly saved")}
if(nf==3) return()
}
if(length(which(paste(getwd(),list.files(getwd()),sep="/")==paste(strsplit(filename.RF,".Rdata")[[1]],".fitSFR",sep="")))==0){
file.rename(from=paste(strsplit(filename.RF,".Rdata")[[1]],".temp",sep=""), to=paste(strsplit(filename.RF,".Rdata")[[1]],".fitSRF",sep=""))
print("successfuly saved")
}
print(paste("Best forest is number",best,"out of",SRF_ini_nbforest_fitted))
print(SRF_ini_OOB_fitted)
correlation.RF.init.vs.fitted()
print("Successfully done")
if (is.null(dev.list())==FALSE){for (i in 1:length(dev.list())){dev.off(dev.list()[1])}}
tkdestroy(tt)
RandForestGUI()
tk2notetab.select(nb, "Initial SRF")
tkfocus(tt)
}
if(exists("SRF_fit_prediction")) {
choiceGUI("Supervised random forest model with to sufficient probes","What do you want to do ?","1:see OOB and confusion table, 2:redo random forest, 3:plot random forest dissimilarities, 4:plot correlation")
nff<<-nf
if(nff==1){prediction()}
if(nff==2){RF_temp();prediction()}

if(nff==4) correlation.RF.init.vs.fitted()
if(nff==3){mds.fitted.RF()}

if (nff>=5) return()
}
if(!exists("SRF_fit_prediction")) {RF_temp();prediction()}
}