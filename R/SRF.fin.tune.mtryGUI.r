###############################
###############################
##  Procedure to tune the mtry in the final random forest
##  Input: NA
##  Output: NA
###############################
###############################
"SRF.fin.tune.mtryGUI"<-function(){
dev_delete()
temp<-function(){
bestmtry<-function(){
mtrym<- round(sqrt(ncol(mat.analyse.RF.fitted)))
a_m<-vector(length=SRF_fin_tunemtry_inc)
a_p<-a_m
for (i in 1:SRF_fin_tunemtry_inc){
a_m[i]<-mtrym-i
a_p[i]<-mtrym+i}
mtry<-sort(c(a_m,a_p,mtrym))
aqq<-which(mtry<=0)
if (length(aqq)!=0){mtry<-mtry[-aqq]}
mtry<<-mtry
OOB<-data.frame(paste("mtry",mtry,sep=""),mtry,vector(length=length(mtry)))
colnames(OOB)<-c("mtryC","mtryN","OOB")
OOB<<-OOB
dat<<-data.frame(mat.analyse.RF.fitted,fact.temp)
for (k in 1:length(mtry)){
fff<<-fff+1 
waitGUI(fff,c(length(mtry)*SRF_fit_tunemtry_nbforest)) 
print(paste("test mtry at",mtry[k]))
mycontrols <- cforest_unbiased(ntree=SRF_fin_tunemtry_nbtree, mtry=mtry[k], minsplit=5)
set.seed(2908)
if (exists("mycforest.temp")) {rm(mycforest.temp);gc(reset=TRUE)}
mycforest.temp <- cforest(formula(paste(names(dat)[length(dat)],"~.")), data=dat, controls=mycontrols)
OOB[k,3]<-c(1-c(sum(fact.temp==predict(mycforest.temp))/nrow(dat)))#%
OOB<<-OOB
}
OOB<<-OOB}
choiceGUI("Best mtry determination","What is your categorical variable ?",paste(colnames(fact.RF),collapse=","))
fact.temp<<-fact.RF[,nf]
choiceGUI("Best mtry determination","How many forests ?","10 is advised")
SRF_fit_tunemtry_nbforest<<-nf
choiceGUI("Best mtry determination","How many trees per forest ?","5000 is advised")
nbtree.temp<<-nf
SRF_fin_tunemtry_nbtree<<-nbtree.temp
choiceGUI("Best mtry determination","What is your mtry incrementation ?","10 is advised")
SRF_fin_tunemtry_inc<<-nf
print("please wait while loading")
valmin<-round(sqrt(ncol(mat.analyse.RF.fitted)))-SRF_fin_tunemtry_inc
if(valmin>=1) valfin<-0
if(valmin==0) valfin<-1
if(valmin<=-1) valfin<-abs(valmin)+1
valfin<-c(length(rep(1,c((SRF_fin_tunemtry_inc*2)+1))))-valfin
val_temp<-matrix(ncol=SRF_fit_tunemtry_nbforest,nrow=valfin)#
colnames(val_temp)<-paste("forest #",1:SRF_fit_tunemtry_nbforest)#
rownames(val_temp)<-paste("mtry #",1:valfin) #
val_temp<<-val_temp #
fff<<-0 #
for (i in 1:SRF_fit_tunemtry_nbforest){#
print(paste("forest #",i))#
SRF_ini_mtry_fitted<-bestmtry()
SRF_ini_mtry_fitted<<-SRF_ini_mtry_fitted
val_temp[,i]<-SRF_ini_mtry_fitted[,3] #
val_temp<<-val_temp#
}#
SRF_fit_tunemtry_restree<<-val_temp#
SRF_ini_mtry_fitted[,3]<-apply(val_temp,1,min)#
SRF_ini_mtry_fitted<<-SRF_ini_mtry_fitted#
fig_par(typee=1)
plot(SRF_ini_mtry_fitted[,2],SRF_ini_mtry_fitted[,3],type="b",ylab="Out-of-bag error",xlab="mtry")
abline(v=round(sqrt(ncol(mat.analyse.RF.fitted))),col="red")
print("successfully done")
}
if(exists("SRF_fit_tunemtry_nbforest")) {choiceGUI("Best mtry determination","What do you want to do","1:plot mtry curve, 2:recalculate mtry, 3:cancel")
if(nf==1){
fig_par(typee=1)
plot(SRF_ini_mtry_fitted[,2],SRF_ini_mtry_fitted[,3],type="b",ylab="Out-of-bag error",xlab="mtry")
abline(v=round(sqrt(ncol(mat.analyse.RF.fitted))),col="red")
print(SRF_ini_mtry_fitted)
}
if(nf==2){ temp()}
if(nf==3){ stop()}
if(nf>=4) { stop()}
}
if(!exists("SRF_fit_tunemtry_nbforest")) temp()
}
