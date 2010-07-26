###############################
###############################
##  Backward probe selection algorithm by iteratively eliminating the least important probes (according to the mean decrease in accuracy). Do it one time from all probes to one probe
##  Input: NA
##  Output: NA
###############################
###############################
"new.SRF.OOB"<-function(){
dev_delete()
temp<-function(){
choiceGUI("Backward taxonomic units selection","What is your categorical variable ?",paste(colnames(fact.RF),collapse=","))
fact.temp<<-fact.RF[,nf]
nbfor.temp<<-1
choiceGUI("Backward taxonomic units selection","How many trees per forest ?","1000 is advised")
nbtree.temp<<-nf
decrease_nb_var_nbtree<<-nbtree.temp
numb_temp_min<<-1
numb_temp_max<<-ncol(mat.analyse.RF)
####################
##partie kruskal
####################
print("Please wait while working")
Kruskal_Pvalue<-vector(length=ncol(mat.analyse.RF))
names(Kruskal_Pvalue)<-names( sort(SRF_ini_myvarimp))
val<-vector(length=length(SRF_ini_myvarimp))
for (i in 1: length(sort(SRF_ini_myvarimp))){
val[i]<-which(colnames(mat.analyse.RF)==names( sort(SRF_ini_myvarimp))[i])}
for(i in 1:length(val)){
temp<-kruskal_test(mat.analyse.RF[,val[i]]~fact.temp,distribution = approximate(B = 9999))
Kruskal_Pvalue[i]<-pvalue(temp)[1]
}
Kruskal_Pvalue<<-Kruskal_Pvalue
###############"""
##calcul du tableau des moyennes
#################
probee<<-sort(SRF_ini_myvarimp)
factt<-fact.temp
finn<-matrix(nc=length(levels(factt))*2,nr=length(probee));
rownames(finn)<-names(probee)
colnames(finn)<-c(paste("mean",levels(factt)),paste("sd",levels(factt)))
for (i in 1:length(probee)){
val<-which(colnames(mat.analyse.RF)==names(probee)[i])
s=split(mat.analyse.RF[,val],factt)
z<-0
for (k in 1: length(s)){
z<-z+1
finn[i,z]<-mean(s[[k]])
}
for (k in 1: length(s)){
z<-z+1
finn[i,z]<-sd(s[[k]])
}
}
finn<-finn
#####################"
##partie iteration
##################"""""
SRF_ini_myvarimp<-sort(SRF_ini_myvarimp)
mat3_temp<-colnames(mat.analyse.RF)
list_temp<-vector("list",c(length(SRF_ini_myvarimp)));names(list_temp)<-names(SRF_ini_myvarimp)
list_temp[[1]]<-names(SRF_ini_myvarimp )
for (i in 2:c(length(SRF_ini_myvarimp))){
mat3_temp<-mat3_temp[-which(mat3_temp==names(SRF_ini_myvarimp[i-1]))]
list_temp[[i]]<-mat3_temp}
list_temp<<-list_temp##
max=length(names(SRF_ini_myvarimp))-numb_temp_max+1
min=length(names(SRF_ini_myvarimp))-numb_temp_min+1
finnal<-matrix(nc=nbfor.temp,nr=c(min-max+1))
colnames(finnal)<-paste("forest #",1:nbfor.temp)
finnal_cor<-finnal
zir<<-0
for (j in 1:nbfor.temp){
for (i in max:min){
zir<<-zir+1
waitGUI(zir,c(nbfor.temp*c(min-max+1)))
dat<-data.frame(mat.analyse.RF[,list_temp[[i]]],factor(unlist(fact.temp)))
print( paste("Remain :",c(min-i+1),"taxonomic units out of",c(min-max+1),". Taxonomic unit",names(list_temp)[i],"deleted."))
mycontrols <- cforest_unbiased(ntree=decrease_nb_var_nbtree, mtry=sqrt(c(ncol(dat)-1)), minsplit=5)#####
set.seed(2908)
mycforest <- cforest(formula(paste(names(dat)[length(dat)],"~.")), data=dat, controls=mycontrols)
OOB<-c(1-c(sum(factor(unlist(fact.temp))==predict(mycforest))/c(nrow(dat)))) #
finnal[c(i-max+1),j]<-OOB
finnal<<-finnal
finnal_cor[c(i-max+1),j]<-c(cor.test(SRF.init.prox.raw,proximity(mycforest))$estimate)^2
finnal_cor<<-finnal_cor
if (exists("mycforest")) {rm(mycforest);gc(reset=TRUE)}
}}
finnal1<<-finnal
finnal_cor1<<-finnal_cor
##################
###creation de  SRF_ini_tune_nb_var
###################
SRF_ini_tune_nb_var<-data.frame( length(SRF_ini_myvarimp):1,round(sqrt(length(SRF_ini_myvarimp):1)),finnal1,finnal_cor1,sort(SRF_ini_myvarimp))
colnames(SRF_ini_tune_nb_var)<-c("Number of probes","mtry","OOB","R2","MeanDecreaseAccuracy")
SRF_ini_tune_nb_var<-cbind(SRF_ini_tune_nb_var,Kruskal_Pvalue,finn)
SRF_ini_tune_nb_var<<-SRF_ini_tune_nb_var
fig_par(typee=1)
plot(SRF_ini_tune_nb_var[,1],SRF_ini_tune_nb_var[,4],type="l",xlab="number of taxonomic units", ylab="pearson R2",main="Correlation between dissimilarities of initial supervised random forest and supervised random forest with the top sufficient probes")##plot OOB
x11()
fig_par(typee=1)
plot(SRF_ini_tune_nb_var[,1],SRF_ini_tune_nb_var[,3],type="l",xlab="number of taxonomic units", ylab="Out-of-bag error",main="Out-of-bag of supervised random forest based on backward taxonomic units elimination")##plot mtry
}
###############################"
if(exists("SRF_ini_tune_nb_var")){
if(!exists("SRF_ini_tune_nb_var_aff")){
choiceGUI("Backward taxonomic units selection","What do you want to do","1:improve the analysis with finer tuning parameters, 2:print and plot result, 3:recalculate, 4:cancel")
nf5<<-nf
if(nf5==4){stop()}
if(nf5==1){new.SRF.OOB.aff()}

if(nf5==2){
fig_par(typee=1)
plot(SRF_ini_tune_nb_var[,1],SRF_ini_tune_nb_var[,4],type="l",xlab="number of taxonomic units", ylab="pearson R2",main="Correlation between dissimilarities of initial supervised random forest and supervised random forest with the top sufficient probes")##plot OOB
x11()
fig_par(typee=1)
plot(SRF_ini_tune_nb_var[,1],SRF_ini_tune_nb_var[,3],type="l",xlab="number of taxonomic units", ylab="Out-of-bag error",main="Out-of-bag of supervised random forest based on backward taxonomic units elimination")##plot mtry
}
if(nf5==3){temp();tkmessageBox(message="You can improve the backward selection results using the 1st choice of the 'Backward taxonomic units selection' button") }
}

if(exists("SRF_ini_tune_nb_var_aff")){new.SRF.OOB.aff()}
}
if(!exists("SRF_ini_tune_nb_var")){temp();tkmessageBox(message="You can improve the backward selection results using the 1st choice of the 'Backward taxonomic units selection' button")   }
}
