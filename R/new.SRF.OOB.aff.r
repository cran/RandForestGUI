###############################
###############################
##  Backward taxonomic units selection algorithm by iteratively eliminating the least important taxonomic units (according to the mean decrease in accuracy). Do it several time on a determined range of taxonomic units to be sure of the analysis
##  Input: NA
##  Output: NA
###############################
###############################
"new.SRF.OOB.aff"<-function(){
temp<-function(){
choiceGUI("Backward taxonomic units selection","What is your categorical variable ?",paste(colnames(fact.RF),collapse=","))
fact.temp<<-fact.RF[,nf]
choiceGUI("Backward taxonomic units selection","How many forests ?","10 is advised")
nbfor.temp<<-nf
decrease_nb_var_nbfor_aff<<-nbfor.temp
choiceGUI("Backward taxonomic units selection","How many trees per forest ?","1000 is advised")
nbtree.temp<<-nf
decrease_nb_var_nbtree_aff<<-nbtree.temp
plot.adjust.numb.var1<-function(){
##choose taxonomic unit
fig_par(typee=1)
plot(SRF_ini_tune_nb_var[,1],SRF_ini_tune_nb_var[,4],type="l",xlab="number of taxonomic unit", ylab="pearson R2",main="Correlation between dissimilarities of initial supervised random forest and supervised random forest with the top sufficient probes")##plot OOB
x11()
fig_par(typee=1)
plot(SRF_ini_tune_nb_var[,1],SRF_ini_tune_nb_var[,3],type="l",xlab="number of taxonomic unit", ylab="Out-of-bag error",main="Out-of-bag of supervised random forest based on backward taxonomic units elimination")##plot mtry
choiceGUI("Backward taxonomic units selection","Choose the minimum x value to perform the finer backward taxonomic unit selection test","")
numb_temp<<-nf
dev.off()
dev.off()
fig_par(typee=1)
plot(SRF_ini_tune_nb_var[,1],SRF_ini_tune_nb_var[,4],type="l",xlab="number of taxonomic unit", ylab="pearson R2",main="Correlation between dissimilarities of initial supervised random forest and supervised random forest with the top sufficient probes")##plot OOB
abline(v=nf,col="red")
x11()
fig_par(typee=1)
plot(SRF_ini_tune_nb_var[,1],SRF_ini_tune_nb_var[,3],type="l",xlab="number of taxonomic unit", ylab="Out-of-bag error",main="Out-of-bag of supervised random forest based on backward taxonomic units elimination")##plot mtry
abline(v=nf,col="red")}
plot.adjust.numb.var2<-function(){
##choose taxonomic units number
fig_par(typee=1)
plot(SRF_ini_tune_nb_var[,1],SRF_ini_tune_nb_var[,4],type="l",xlab="number of taxonomic units", ylab="pearson R2",sub="in dashed blue, the minimum x value",main="Correlation between dissimilarities of initial supervised random forest and supervised random forest with the top sufficient probes")##plot OOB
abline(v=numb_temp_min,col="blue",lty=2)
x11()
fig_par(typee=1)
plot(SRF_ini_tune_nb_var[,1],SRF_ini_tune_nb_var[,3],type="l",xlab="number of taxonomic units", ylab="OOB error",sub="in dashed blue, the minimum x value",main="Out-of-bag of supervised random forest based on backward taxonomic units elimination")##plot mtry
abline(v=numb_temp_min,col="blue",lty=2)
choiceGUI("Backward taxonomic units selection","Choose maximum x value to perform the finer backward taxonomic unit selection test","")
numb_temp<<-nf
dev.off()
dev.off()
fig_par(typee=1)
plot(SRF_ini_tune_nb_var[,1],SRF_ini_tune_nb_var[,4],type="l",xlab="number of taxonomic units", ylab="pearson R2",main="Correlation between dissimilarities of initial supervised random forest and supervised random forest with the top sufficient probes")##plot OOB
abline(v=numb_temp_min,col="blue",lty=2)
abline(v=nf,col="red")
x11()
fig_par(typee=1)
plot(SRF_ini_tune_nb_var[,1],SRF_ini_tune_nb_var[,3],type="l",xlab="number of taxonomic units", ylab="OOB error",main="Out-of-bag of supervised random forest based on backward taxonomic units elimination")##plot mtry
abline(v=nf,col="red")
abline(v=numb_temp_min,col="blue",lty=2)
}
plot.adjust.numb.var1()
choiceGUI("Backward taxonomic units selection","Redefine the minimum x value to perform the finer backward taxonomic unit selection test ?","1:Redefine, 2:validate, 3:cancel")
dev.off()
dev.off()
if(nf==1) plot.adjust.numb.var1()
if(nf==2){
numb_temp_min<<-numb_temp+1
plot.adjust.numb.var2()
}
if(nf==3) return()
choiceGUI("Backward taxonomic units selection","Redefine the maximum x value to perform the finer backward taxonomic unit selection test ?","1:Redefine, 2:validate, 3:cancel")
dev.off()
dev.off()
if(nf==1) plot.adjust.numb.var2()
if(nf==2){
numb_temp_max<<-numb_temp+1
}
if(nf==3) return()
SRF_ini_myvarimp<-sort(SRF_ini_myvarimp)
mat3_temp<-colnames(mat.analyse.RF)
list_temp<-vector("list",c(length(SRF_ini_myvarimp)));names(list_temp)<-names(SRF_ini_myvarimp)
list_temp[[1]]<-SRF_ini_myvarimp
for (i in 2:c(length(SRF_ini_myvarimp))){
mat3_temp<-mat3_temp[-which(mat3_temp==names(SRF_ini_myvarimp[i-1]))]
list_temp[[i]]<-mat3_temp}
max=length(names(SRF_ini_myvarimp))-numb_temp_max+1
min=length(names(SRF_ini_myvarimp))-numb_temp_min+1
finnal<-matrix(nc=decrease_nb_var_nbfor_aff,nr=c(min-max+1))
colnames(finnal)<-paste("forest #",1:decrease_nb_var_nbfor_aff)
finnal_cor<-finnal
zir<<-0
for (j in 1:decrease_nb_var_nbfor_aff){
print(paste("forest #",j))
for (i in max:min){
zir<<-zir+1
waitGUI(zir,c(decrease_nb_var_nbfor_aff*c(min-max+1)))
dat<-data.frame(mat.analyse.RF[,list_temp[[i]]],factor(unlist(fact.temp)))
print( paste("Remain :",c(min-i+1),"taxonomic units out of",c(min-max+1),". Taxonomic unit",names(list_temp)[i],"deleted."))
mycontrols <- cforest_unbiased(ntree=decrease_nb_var_nbtree_aff, mtry=sqrt(c(ncol(dat)-1)), minsplit=5)#####
set.seed(2908)
mycforest <- cforest(formula(paste(names(dat)[length(dat)],"~.")), data=dat, controls=mycontrols)
OOB<-c(1-c(sum(factor(unlist(fact.temp))==predict(mycforest))/c(nrow(dat)))) #
finnal[c(i-max+1),j]<-OOB
finnal<<-finnal
finnal_cor[c(i-max+1),j]<-c(cor.test(SRF.init.prox.raw,proximity(mycforest))$estimate)^2
finnal_cor<<-finnal_cor
if (exists("mycforest")) {rm(mycforest);gc(reset=TRUE)}
}}
SRF_ini_tune_nb_var_aff_maxmin<<-c(max,min)
SRF_ini_tune_nb_var_aff<-finnal;SRF_ini_tune_nb_var_aff[]<-NA
for(i in 1:nrow(finnal)){
SRF_ini_tune_nb_var_aff[i,]<-finnal[c(nrow(finnal)-i+1),]
}
SRF_ini_tune_nb_var_aff<<-SRF_ini_tune_nb_var_aff
SRF_ini_tune_nb_var_aff_cor<-finnal_cor;SRF_ini_tune_nb_var_aff_cor[]<-NA
for(i in 1:nrow(finnal_cor)){
SRF_ini_tune_nb_var_aff_cor[i,]<-finnal_cor[c(nrow(finnal_cor)-i+1),]
}
SRF_ini_tune_nb_var_aff_cor<<-SRF_ini_tune_nb_var_aff_cor
#xlimit=c(0,length(SRF_ini_tune_nb_var[,1]))
#ylimit=c(min(SRF_ini_tune_nb_var[,3]),max(SRF_ini_tune_nb_var[,3]))
#aa=c(min(apply(SRF_ini_tune_nb_var,1,min)),max(apply(SRF_ini_tune_nb_var,1,min)));ylimit=c(min(c(ylimit[1],aa[1])),max(c(ylimit[2],aa[2])))
#fig_par(typee=1)
#plot(SRF_ini_tune_nb_var[,1],SRF_ini_tune_nb_var[,3],xlim=xlimit,ylim=ylimit,type="l",xlab="number of taxonomic units", ylab="Out-of-bag error",main="Out-of-bag of supervised random forest based on backward taxonomic units elimination",sub="mtry+-5,5000 trees, cforest_unbiased")##plot mtry
#par(new=TRUE)
#plot(c(c(c(length(names(SRF_ini_myvarimp)))-SRF_ini_tune_nb_var_aff_maxmin[2]):c(length(names(SRF_ini_myvarimp))-SRF_ini_tune_nb_var_aff_maxmin[1])),apply(SRF_ini_tune_nb_var_aff,1,min),type="l",xlab=NA,ylab=NA,col="red",xlim=xlimit,ylim=ylimit)
#x11()
#fig_par(typee=1)
#xxlimit=c(0,length(SRF_ini_tune_nb_var[,1]))
#yylimit=c(min(SRF_ini_tune_nb_var[,4]),max(SRF_ini_tune_nb_var[,4]))
#plot(SRF_ini_tune_nb_var[,1],SRF_ini_tune_nb_var[,4],type="l",xlab="number of taxonomic units", ylab="Pearson R2",main="Correlation between dissimilarities of initial supervised random forest and supervised random forest with the top sufficient probes",xlim=xxlimit,ylim=yylimit)##plot OOB
#if (exists("SRF_ini_tune_nb_var_aff_cor")){
#par(new=TRUE)
#plot(c(c(c(length(names(SRF_ini_myvarimp)))-SRF_ini_tune_nb_var_aff_maxmin[2]):c(length(names(SRF_ini_myvarimp))-SRF_ini_tune_nb_var_aff_maxmin[1])),apply(SRF_ini_tune_nb_var_aff_cor,1,mean),type="l",xlab=NA,ylab=NA,col="red",xlim=xxlimit,ylim=yylimit)
#}
}
plot_temp<-function(){
xlimit=c(0,length(SRF_ini_tune_nb_var[,1]))
ylimit=c(min(SRF_ini_tune_nb_var[,3]),max(SRF_ini_tune_nb_var[,3]))
aa=c(min(apply(SRF_ini_tune_nb_var_aff,1,min)),max(apply(SRF_ini_tune_nb_var_aff,1,min)));ylimit=c(min(c(ylimit[1],aa[1])),max(c(ylimit[2],aa[2])))
fig_par(typee=1)
plot(SRF_ini_tune_nb_var[,1],SRF_ini_tune_nb_var[,3],xlim=xlimit,ylim=ylimit,type="l",xlab="number of taxonomic units", ylab="Out-of-bag error",main="Out-of-bag of supervised random forest based on backward taxonomic units elimination",sub="mtry+-5,5000 trees, cforest_unbiased")##plot mtry
par(new=TRUE)
plot(c(c(c(length(names(SRF_ini_myvarimp)))-SRF_ini_tune_nb_var_aff_maxmin[2]):c(length(names(SRF_ini_myvarimp))-SRF_ini_tune_nb_var_aff_maxmin[1])),apply(SRF_ini_tune_nb_var_aff,1,min),type="l",xlab=NA,ylab=NA,col="red",xlim=xlimit,ylim=ylimit)
x11()
xxlimit=c(0,length(SRF_ini_tune_nb_var[,1]))
yylimit=c(min(SRF_ini_tune_nb_var[,4]),max(SRF_ini_tune_nb_var[,4]))
aa=c(min(apply(SRF_ini_tune_nb_var_aff_cor,1,min)),max(apply(SRF_ini_tune_nb_var_aff_cor,1,min)));yylimit=c(min(c(yylimit[1],aa[1])),max(c(yylimit[2],aa[2])))
fig_par(typee=1)
plot(SRF_ini_tune_nb_var[,1],SRF_ini_tune_nb_var[,4],type="l",xlab="number of taxonomic units", ylab="Pearson R2",main="Correlation between dissimilarities of initial supervised random forest and supervised random forest with the top sufficient probes",xlim=xxlimit,ylim=yylimit)##plot OOB
if (exists("SRF_ini_tune_nb_var_aff_cor")){
par(new=TRUE)
plot(c(c(c(length(names(SRF_ini_myvarimp)))-SRF_ini_tune_nb_var_aff_maxmin[2]):c(length(names(SRF_ini_myvarimp))-SRF_ini_tune_nb_var_aff_maxmin[1])),apply(SRF_ini_tune_nb_var_aff_cor,1,mean),type="l",xlab=NA,ylab=NA,col="red",xlim=xxlimit,ylim=yylimit)
}
}
###############################"
nf4<<-0
if(exists("SRF_ini_tune_nb_var_aff")){
choiceGUI("Supervised Random Forest","What to do ?","1 plot and print result, 2 recalculate, 3:cancel")
nf4<<-nf
if(nf4==1){plot_temp()}
if(nf4==2){temp();plot_temp()}
if(nf4==3){stop()}
}
if(!exists("SRF_ini_tune_nb_var_aff")){temp()  }
}
