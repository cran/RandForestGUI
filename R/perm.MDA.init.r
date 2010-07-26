###############################
###############################
##  Calculates Monte Carlo permutations on taxonomic units mean decrease accuracy
##  Input: NA
##  Output: NA
###############################
###############################
"perm.MDA.init"<-function(){
dev_delete()
temp<-function(){
choiceGUI("Significance of taxonomic units importance","What is your categorical variable ?",paste(colnames(fact.RF),collapse=","))
fact.temp<<-fact.RF[,nf]
choiceGUI("Significance of taxonomic units importance","Choose number of permutation","10000 is advised")
perm.number<<-nf
choiceGUI("Significance of taxonomic units importance","Choose number of tree","10000 is advised")
perm.nbtree.temp<<-nf
library("party")
print("Please wait while working")
mycontrols <- cforest_unbiased(ntree=perm.nbtree.temp, mtry=round(sqrt(dim(mat.analyse.RF)[2])), minsplit=5)
##calcul des valeurs réelles
set.seed(2908)
dat<-data.frame(mat.analyse.RF,fact.temp)
SRF_ini<- cforest(formula(paste(names(dat)[length(dat)],"~.")), data=dat, controls=mycontrols)
reel<-varimp(SRF_ini)
##calcul des permutations
val<-matrix(nr=perm.number,nc=dim(mat.analyse.RF)[2])
colnames(val)<-colnames(mat.analyse.RF)
for (i in 1:perm.number){#début
if (exists("forest")) {rm(forest);gc(reset=TRUE)}
waitGUI(i,perm.number)
print(paste("iteration #",i,"out of",perm.number))
set.seed(29083)
fact.temp<-sample(fact.temp,length(fact.temp))
dat<-data.frame(mat.analyse.RF,fact.temp)
forest<- cforest(formula(paste(names(dat)[length(dat)],"~.")), data=dat, controls=mycontrols)
val[i,]<-varimp(forest)}
###calcul des pvalue
pval<-reel;pval[]<-NA
for (i in 1:length(reel)){
pval[i]<-length(which(val[,i]>reel[i]))/dim(val)[1]
}
dev.off()
print("Successfully done")
names(pval)<-colnames(mat.analyse.RF)
pval.init<-pval
ess<-pval
for (i in 1:length(pval.init)){ess[i]<-which(names(pval.init)==names(SRF_ini_myvarimp)[i])}
pval.init<<-pval.init[ess]
print("p values of taxonomic units based on Monte Carlo permutations")
print(pval.init)
}
if (exists("pval.init")) {
choiceGUI("Significance of taxonomic units importance","What do you want to do?","1:recalculate p values, 2:print p values")
if(nf==1) {temp()}
if(nf==2)  {print("p values of taxonomic units based on Monte Carlo permutations");print(pval.init)}
}
if (!exists("pval.init")) {temp()}
}