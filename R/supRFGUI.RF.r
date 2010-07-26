###############################
###############################
##  Build supervised random forest model 
##  Input: NA
##  Output: NA
###############################
###############################
"supRFGUI.RF"<-function(){
dev_delete()
prediction<-function(){
print("Complete error table made by the supervised random forest")
print(SRF_ini_prediction[[1]])
print("Confusion matrix made by the supervised random forest")
print(SRF_ini_prediction[[2]])
print("Out-of-bag of the supervised random forest (% of misclassification)")
print(SRF_ini_prediction[[3]])
}
RF_temp<-function(){
choiceGUI("Supervised random forest","What is your categorical variable ?",paste(colnames(fact.RF),collapse=","))
fact.temp<<-fact.RF[,nf]
fact.supRF.init<<-colnames(fact.RF)[nf]
choiceGUI("Supervised random forest","How many forests ?","10 advised")
SRF_ini_nbforest<<-nf#
choiceGUI("Supervised random forest","How many trees per forest","10000 is advised")
nbtree.temp<<-nf
SRF.init.nbtree<<-nbtree.temp
if(exists("SRF_ini_mtry")) choiceGUI("Supervised random forest","What is the mtry ?",paste("Please see the result with the 'Best mtry determination' button; otherwise heuristic value",round(sqrt(ncol(mat.analyse.RF)))))
if(!exists("SRF_ini_mtry"))choiceGUI("Supervised random forest","What is the mtry ?",paste("Heuristic formula advises",round(sqrt(ncol(mat.analyse.RF))),"but the 'Best mtry determination' button provides better results."))
mtry.temp<<-nf
SRF.init.mtry<<-mtry.temp
library("party")
mycontrols <- cforest_unbiased(ntree=nbtree.temp, mtry=mtry.temp, minsplit=5)
mycontrols <<-mycontrols
set.seed(2908)
dat<-data.frame(mat.analyse.RF,fact.temp)
SRF_ini_dat<<-dat
print("Pleased wait while working")
SRF_ini_OOB<<-1
for (i in 1:SRF_ini_nbforest){
waitGUI(i,SRF_ini_nbforest)
if(exists("SRF_ini")){rm(SRF_ini);gc(reset=TRUE) } #
SRF_ini<- cforest(formula(paste(names(dat)[length(dat)],"~.")), data=dat, controls=mycontrols) ##
OOB<<-c(1-c(sum(SRF_ini_dat$fact.temp==predict(SRF_ini))/nrow(SRF_ini_dat)))#% of correct prediction <=> OOB estimate of  error rate
print(paste("0ut-of-bag of iteration",i,"is of",OOB))
if (OOB<SRF_ini_OOB){
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
save(SRF_ini,file=paste(strsplit(filename.RF,".Rdata")[[1]],".temp",sep=""))
SRF_ini_OOB<<-OOB
SRF.init.prox.raw<<-proximity(SRF_ini)
aa<-data.frame(SRF_ini_dat$fact.temp==predict(SRF_ini),SRF_ini_dat$fact.temp,predict(SRF_ini))
colnames(aa)<-c("Prediction","Real factor","Predicted factor")
rownames(aa)<-rownames(SRF_ini_dat)
complete_table<-aa
conf<-table(SRF_ini_dat$fact.temp,predict(SRF_ini))
confusion_matrix<-conf
SRF_ini_prediction<<-list(complete_table,confusion_matrix,SRF_ini_OOB)
best<<-i
}
if(exists("SRF_ini")){rm(SRF_ini);gc(reset=TRUE) } #
}
##save
if(length(which(paste(getwd(),list.files(getwd()),sep="/")==paste(strsplit(filename.RF,".Rdata")[[1]],".iniSFR",sep="")))==1){
choiceGUI("Save","Replace existing file","1:no, 2:yes, 3:cancel")
if(nf==1) return()
if(nf==2) {
file.rename(from=paste(strsplit(filename.RF,".Rdata")[[1]],".temp",sep=""), to=paste(strsplit(filename.RF,".Rdata")[[1]],".iniSRF",sep=""))
print("successfuly saved")}
if(nf==3) return()
}
if(length(which(paste(getwd(),list.files(getwd()),sep="/")==paste(strsplit(filename.RF,".Rdata")[[1]],".iniSRF",sep="")))==0){ 
file.rename(from=paste(strsplit(filename.RF,".Rdata")[[1]],".temp",sep=""), to=paste(strsplit(filename.RF,".Rdata")[[1]],".iniSRF",sep=""))
print("Successfuly saved")
 }
print(paste("Best forest is number",best,"out of",SRF_ini_nbforest))
print(SRF_ini_OOB)
print("Successfully done")
if (is.null(dev.list())==FALSE){for (i in 1:length(dev.list())){dev.off(dev.list()[1])}}
tkdestroy(tt)
RandForestGUI()
tk2notetab.select(nb, "Final SRF")
tkfocus(tt)
}
if(exists("SRF_ini_prediction")) {choiceGUI("Supervised random forest","What do you want to do ?","1:see OOB and confusion table, 2:redo random forest, 3:plot random forest dissimilarities, 4:cancel")
nff<<-nf
if(nff==1){prediction()}
if(nff==2){RF_temp();prediction()}
if(nff==3){mds.RF()}
if (nff>=4) stop()
}
if(!exists("SRF_ini_prediction")) {RF_temp();prediction();tkmessageBox(message="You can plot supervised random forest by clicking again on the 'Calculate/plot supervised random forest' button")}
}