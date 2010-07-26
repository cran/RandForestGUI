###############################
###############################
##  Procedure to choose the number of taxonomic units selected in the final random forest
##  Input: NA
##  Output: NA
###############################
###############################
"plot_adjust.numb.var"<-function(){
dev_delete()
temp<-function(){
xxlimit=c(0,length(SRF_ini_tune_nb_var[,1]))
yylimit=c(min(SRF_ini_tune_nb_var[,4]),max(SRF_ini_tune_nb_var[,4]))
fig_par(typee=1)
plot(SRF_ini_tune_nb_var[,1],SRF_ini_tune_nb_var[,4],type="l",xlab="Number of taxonomic units", ylab="Pearson R2",main="Correlation between dissimilarities of initial supervised random forest and supervised random forest with the top sufficient probes",xlim=xxlimit,ylim=yylimit)##plot Out-ofbag
if (exists("SRF_ini_tune_nb_var_aff_cor")){
par(new=TRUE)
plot(c(c(c(length(names(SRF_ini_myvarimp)))-SRF_ini_tune_nb_var_aff_maxmin[2]):c(length(names(SRF_ini_myvarimp))-SRF_ini_tune_nb_var_aff_maxmin[1])),apply(SRF_ini_tune_nb_var_aff_cor,1,mean),type="l",xlab=NA,ylab=NA,col="red",xlim=xxlimit,ylim=yylimit)
}
x11()
xlimit=c(0,length(SRF_ini_tune_nb_var[,1]))
ylimit=c(min(SRF_ini_tune_nb_var[,3]),max(SRF_ini_tune_nb_var[,3]))
fig_par(typee=1)
plot(SRF_ini_tune_nb_var[,1],SRF_ini_tune_nb_var[,3],xlim=xlimit,ylim=ylimit,type="l",xlab="Number of taxonomic units", ylab="Out-of-bag error",main="Out-of-bag of supervised random forest based on backward taxonomic units elimination")##plot mtry
if (exists("SRF_ini_tune_nb_var_aff_maxmin")){
par(new=TRUE)
plot(c(c(c(length(names(SRF_ini_myvarimp)))-SRF_ini_tune_nb_var_aff_maxmin[2]):c(length(names(SRF_ini_myvarimp))-SRF_ini_tune_nb_var_aff_maxmin[1])),apply(SRF_ini_tune_nb_var_aff,1,min),type="l",xlab=NA,ylab=NA,col="red",xlim=xlimit,ylim=ylimit)
}
print(SRF_ini_tune_nb_var[,1:6])
choiceGUI("Selection of top sufficient taxonomic units","Choose a number of taxonomic units","")
numb_temp<<-nf
dev.off()
dev.off()
fig_par(typee=1)
plot(SRF_ini_tune_nb_var[,1],SRF_ini_tune_nb_var[,4],type="l",xlab="Number of taxonomic units", ylab="Pearson R2",main="Correlation between dissimilarities of initial supervised random forest and supervised random forest with the top sufficient probes")##plot Out-of-bag
abline(v=nf,col="red")
abline(v=nf-1,col="blue")
abline(v=nf+1,col="blue")
x11()
fig_par(typee=1)
plot(SRF_ini_tune_nb_var[,1],SRF_ini_tune_nb_var[,3],type="l",xlab="Number of taxonomic units", ylab="Out-of-bag error",main="Out-of-bag of supervised random forest based on backward taxonomic units elimination")##plot mtry
if (exists("SRF_ini_tune_nb_var_aff_maxmin")){
par(new=TRUE)
plot(c(c(c(length(names(SRF_ini_myvarimp)))-SRF_ini_tune_nb_var_aff_maxmin[2]):c(length(names(SRF_ini_myvarimp))-SRF_ini_tune_nb_var_aff_maxmin[1])),apply(SRF_ini_tune_nb_var_aff,1,min),type="l",xlab=NA,ylab=NA,col="red",xlim=xlimit,ylim=ylimit)
}
abline(v=nf,col="red")
abline(v=nf-1,col="blue")
abline(v=nf+1,col="blue")
print(SRF_ini_tune_nb_var[c(c(c(nrow(SRF_ini_tune_nb_var))-numb_temp):c(nrow(SRF_ini_tune_nb_var)-1)),] )
SRF_ini_tune_nb_var_selected<<-SRF_ini_tune_nb_var[c(c(c(nrow(SRF_ini_tune_nb_var))-numb_temp):c(nrow(SRF_ini_tune_nb_var)-1)),]

choiceGUI("Selection of top sufficient taxonomic units","Validation ?","1:redo, 2:validation, 3:cancel")
dev.off()
dev.off()
if(nf==1) temp()
if(nf==2) {
SRF_ini_myvarimp_selected<-SRF_ini_myvarimp[1:numb_temp]
 SRF_ini_myvarimp_selected<<-SRF_ini_myvarimp_selected
ass<-vector(length=length(SRF_ini_myvarimp_selected))
for (i in 1:length(SRF_ini_myvarimp_selected)){
ass[i]<-which(colnames(mat.analyse.RF)==names(SRF_ini_myvarimp_selected)[i]) }
mat.analyse.RF.fitted<-mat.analyse.RF[,ass]
mat.analyse.RF.fitted<<-mat.analyse.RF.fitted
print(SRF_ini_myvarimp_selected)
}
if(nf==3) stop()
}

if(exists("mat.analyse.RF.fitted")) {choiceGUI("Selection of top sufficient taxonomic units","What do you want to do ?","1:redo, 2:cancel")
nff4<<-nf
if(nff4==2){stop()}
if(nff4==1){temp()}
}
if(!exists("mat.analyse.RF.fitted")) temp() 
}