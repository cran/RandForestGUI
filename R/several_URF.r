###############################
###############################
##  Calculate several unsupervised random forest and calculate the mean of unsupervised random forest distance
##  Input: NA
##  Output: NA
###############################
###############################
"several_URF"<-function(){
dev_delete()
several_URF_temp<-function(){
bestforest<-function(){
OBB<-vector(length=URF_nbforest);OBB[]<-NA
OBB<<-OBB
d<-0
d<<-d
prox<-vector("list", URF_nbforest);names(prox)<-paste("iteration",1:URF_nbforest,sep="")
prox<<-prox
for ( k in 1:URF_nbforest){
waitGUI(k,URF_nbforest)
print(paste("forest #",k,"out of",URF_nbforest) )
index1 <- sample(c(1:(2*dim(mat.analyse.RF)[1])))##donne val au hasard 1 à 2* nb observation
rep1 <-  order(index1) ###donne ordre
yy<-vector(length=2*dim(mat.analyse.RF)[1]);yy[]<-2;yy[1:dim(mat.analyse.RF)[1]]<-1;yy<-factor(yy)##crée le facteur
if (URF_meth=="addcl1") sample1 <- function(X) {sample(X, replace=TRUE) }## addcl1
if (URF_meth=="addcl2") sample1 <- function(X)   { runif(length(X), min=min(X), max =max(X)) }## addcl2
dd<-apply(mat.analyse.RF,2,sample1)###permute les lignes
val<-data.frame(yy,rbind(mat.analyse.RF,dd))
datRFsyn<<-val[index1,]
if (exists("mycforesttemp")) {rm(mycforesttemp);gc(reset=TRUE)}
mycontrols <- cforest_unbiased(ntree=URF_nbtree, mtry=URF_mtryy, minsplit=5)
set.seed(2908)
mycforesttemp <- cforest(yy ~ ., data=datRFsyn, controls=mycontrols)
OBB[k]<-c(sum(yy==predict(mycforesttemp))/length(yy))##OOB
RF1prox <- proximity(mycforesttemp)[rep1,rep1]
if (exists("mycforesttemp")) {rm(mycforesttemp);gc(reset=TRUE)}
RF1prox<-RF1prox[c(1:dim(mat.analyse.RF)[1]),c(1:dim(mat.analyse.RF)[1])]
distRFAddcl1 <- as.dist(sqrt(1-RF1prox))
distRFAddcl1[distRFAddcl1<=0] <- 0.0000000000000000000000000000000001
RF1prox=as.matrix(distRFAddcl1)
rownames(RF1prox)<-rownames(mat.analyse.RF)
prox[[k]]<-RF1prox
d<-list(prox,OBB)
d<<-d
}
dev.off()
d<<-d
}
if (!exists("URF_mtry")){choiceGUI("Calculate unsupervised random forest","What is your mtry ?",paste("Heuristic formula advises",ceiling(sqrt(dim(mat.analyse.RF)[2])),"but the 'Best mtry determination' button provides better results."))
URF_mtryy=nf
URF_mtryy<<-URF_mtryy }
if (exists("URF_mtry")){choiceGUI("Calculate unsupervised random forest","What is your mtry ?",paste("Please see the result with the 'Best mtry determination' button; otherwise heuristic value",ceiling(sqrt(dim(mat.analyse.RF)[2]))))
URF_mtryy=nf
URF_mtryy<<-URF_mtryy }
choiceGUI("Calculate unsupervised random forest","How many forests ?","advised 500")
URF_nbforest=nf
URF_nbforest<<-URF_nbforest
choiceGUI("Calculate unsupervised random forest","How many trees per forest ?","advised 5000")
nbtree<-nf
URF_nbtree<<-nbtree
URF<-bestforest()##ici on récup a si intyerompue av sinon aza
URF<<-URF
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
if(length(which(paste(getwd(),list.files(getwd()),sep="/")==paste(strsplit(filename.RF,".Rdata")[[1]],".URF",sep="")))==1){
choiceGUI("Save","Replace existing file","1:no, 2:yes, 3:cancel")
if(nf==1) return()
if(nf==2) {save(URF,file=paste(strsplit(filename.RF,".Rdata")[[1]],".URF",sep=""))
print("successfuly saved")}
if(nf==3) return()
}
if(length(which(paste(getwd(),list.files(getwd()),sep="/")==paste(strsplit(filename.RF,".Rdata")[[1]],".URF",sep="")))==0){
save(URF,file=paste(strsplit(filename.RF,".Rdata")[[1]],".URF",sep=""))
print("successfuly saved")
}
mean<-matrix(nrow=nrow(URF[[1]][[1]]),ncol=ncol(URF[[1]][[1]]));mean[]<-NA;
rownames(mean)<-rownames(URF[[1]][[1]])
colnames(mean)<-rownames(URF[[1]][[1]])
median<-mean
sd<-mean
for (i in 1:nrow(URF[[1]][[1]])){
for (j in 1:ncol(URF[[1]][[1]])){
vec<-vector(length=length(URF[[1]]))
for (l in 1:length(URF[[1]])) {vec[l]<-URF[[1]][[l]][i,j] }
mean[i,j]<-mean(vec)
median[i,j]<-median(vec)
sd[i,j]<-sd(vec)
}}
URF_mean<-as.dist(mean);URF_median<-as.dist(median);URF_sd<-as.dist(sd)
URF_mean<<-URF_mean
URF_median<<-URF_median
URF_sd<<-URF_sd
##correlation median moy
z<-cor.test(URF_mean,URF_median)
(z$estimate)^2
z$p.value
fig_par(typee=1)
plot(URF_mean,URF_median,main=paste("Unsup. CI RF between mean and median of all forests dissimilarities:  R2=",(z$estimate)^2,"P=",z$p.value),sub=paste(URF_nbforest,"forests", URF_nbtree,"trees/forest", URF_meth, "cforest_unbiased"))
}

if (exists("URF_mean")){
choiceGUI("Calculate and plot unsupervised random forest","What do you want to do ?","1:recalculate unsupervised random forest,  2:plot unsupervised random forest")
nff1<<-nf
if(nff1==1) {several_URF_temp();tkmessageBox(message="You can plot unsupervised random forest by clicking again on the 'Calculate/plot unsupervised random forest' button")}
if(nff1==2) {mds.URF()}
}
if (!exists("URF_mean")){several_URF_temp();tkmessageBox(message="You can plot unsupervised random forest by clicking again on the 'Calculate/plot unsupervised random forest' button")} 
}

